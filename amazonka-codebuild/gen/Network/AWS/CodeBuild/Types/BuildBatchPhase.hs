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
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchPhase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatchPhase where

import Network.AWS.CodeBuild.Types.BuildBatchPhaseType
import Network.AWS.CodeBuild.Types.PhaseContext
import Network.AWS.CodeBuild.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a stage for a batch build.
--
-- /See:/ 'newBuildBatchPhase' smart constructor.
data BuildBatchPhase = BuildBatchPhase'
  { -- | The name of the batch build phase. Valid values include:
    --
    -- [COMBINE_ARTIFACTS]
    --     Build output artifacts are being combined and uploaded to the output
    --     location.
    --
    -- [DOWNLOAD_BATCHSPEC]
    --     The batch build specification is being downloaded.
    --
    -- [FAILED]
    --     One or more of the builds failed.
    --
    -- [IN_PROGRESS]
    --     The batch build is in progress.
    --
    -- [STOPPED]
    --     The batch build was stopped.
    --
    -- [SUBMITTED]
    --     The btach build has been submitted.
    --
    -- [SUCCEEDED]
    --     The batch build succeeded.
    phaseType :: Prelude.Maybe BuildBatchPhaseType,
    -- | Additional information about the batch build phase. Especially to help
    -- troubleshoot a failed batch build.
    contexts :: Prelude.Maybe [PhaseContext],
    -- | When the batch build phase started, expressed in Unix time format.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | When the batch build phase ended, expressed in Unix time format.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | How long, in seconds, between the starting and ending times of the batch
    -- build\'s phase.
    durationInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The current status of the batch build phase. Valid values include:
    --
    -- [FAILED]
    --     The build phase failed.
    --
    -- [FAULT]
    --     The build phase faulted.
    --
    -- [IN_PROGRESS]
    --     The build phase is still in progress.
    --
    -- [QUEUED]
    --     The build has been submitted and is queued behind other submitted
    --     builds.
    --
    -- [STOPPED]
    --     The build phase stopped.
    --
    -- [SUCCEEDED]
    --     The build phase succeeded.
    --
    -- [TIMED_OUT]
    --     The build phase timed out.
    phaseStatus :: Prelude.Maybe StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BuildBatchPhase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phaseType', 'buildBatchPhase_phaseType' - The name of the batch build phase. Valid values include:
--
-- [COMBINE_ARTIFACTS]
--     Build output artifacts are being combined and uploaded to the output
--     location.
--
-- [DOWNLOAD_BATCHSPEC]
--     The batch build specification is being downloaded.
--
-- [FAILED]
--     One or more of the builds failed.
--
-- [IN_PROGRESS]
--     The batch build is in progress.
--
-- [STOPPED]
--     The batch build was stopped.
--
-- [SUBMITTED]
--     The btach build has been submitted.
--
-- [SUCCEEDED]
--     The batch build succeeded.
--
-- 'contexts', 'buildBatchPhase_contexts' - Additional information about the batch build phase. Especially to help
-- troubleshoot a failed batch build.
--
-- 'startTime', 'buildBatchPhase_startTime' - When the batch build phase started, expressed in Unix time format.
--
-- 'endTime', 'buildBatchPhase_endTime' - When the batch build phase ended, expressed in Unix time format.
--
-- 'durationInSeconds', 'buildBatchPhase_durationInSeconds' - How long, in seconds, between the starting and ending times of the batch
-- build\'s phase.
--
-- 'phaseStatus', 'buildBatchPhase_phaseStatus' - The current status of the batch build phase. Valid values include:
--
-- [FAILED]
--     The build phase failed.
--
-- [FAULT]
--     The build phase faulted.
--
-- [IN_PROGRESS]
--     The build phase is still in progress.
--
-- [QUEUED]
--     The build has been submitted and is queued behind other submitted
--     builds.
--
-- [STOPPED]
--     The build phase stopped.
--
-- [SUCCEEDED]
--     The build phase succeeded.
--
-- [TIMED_OUT]
--     The build phase timed out.
newBuildBatchPhase ::
  BuildBatchPhase
newBuildBatchPhase =
  BuildBatchPhase'
    { phaseType = Prelude.Nothing,
      contexts = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      durationInSeconds = Prelude.Nothing,
      phaseStatus = Prelude.Nothing
    }

-- | The name of the batch build phase. Valid values include:
--
-- [COMBINE_ARTIFACTS]
--     Build output artifacts are being combined and uploaded to the output
--     location.
--
-- [DOWNLOAD_BATCHSPEC]
--     The batch build specification is being downloaded.
--
-- [FAILED]
--     One or more of the builds failed.
--
-- [IN_PROGRESS]
--     The batch build is in progress.
--
-- [STOPPED]
--     The batch build was stopped.
--
-- [SUBMITTED]
--     The btach build has been submitted.
--
-- [SUCCEEDED]
--     The batch build succeeded.
buildBatchPhase_phaseType :: Lens.Lens' BuildBatchPhase (Prelude.Maybe BuildBatchPhaseType)
buildBatchPhase_phaseType = Lens.lens (\BuildBatchPhase' {phaseType} -> phaseType) (\s@BuildBatchPhase' {} a -> s {phaseType = a} :: BuildBatchPhase)

-- | Additional information about the batch build phase. Especially to help
-- troubleshoot a failed batch build.
buildBatchPhase_contexts :: Lens.Lens' BuildBatchPhase (Prelude.Maybe [PhaseContext])
buildBatchPhase_contexts = Lens.lens (\BuildBatchPhase' {contexts} -> contexts) (\s@BuildBatchPhase' {} a -> s {contexts = a} :: BuildBatchPhase) Prelude.. Lens.mapping Prelude._Coerce

-- | When the batch build phase started, expressed in Unix time format.
buildBatchPhase_startTime :: Lens.Lens' BuildBatchPhase (Prelude.Maybe Prelude.UTCTime)
buildBatchPhase_startTime = Lens.lens (\BuildBatchPhase' {startTime} -> startTime) (\s@BuildBatchPhase' {} a -> s {startTime = a} :: BuildBatchPhase) Prelude.. Lens.mapping Prelude._Time

-- | When the batch build phase ended, expressed in Unix time format.
buildBatchPhase_endTime :: Lens.Lens' BuildBatchPhase (Prelude.Maybe Prelude.UTCTime)
buildBatchPhase_endTime = Lens.lens (\BuildBatchPhase' {endTime} -> endTime) (\s@BuildBatchPhase' {} a -> s {endTime = a} :: BuildBatchPhase) Prelude.. Lens.mapping Prelude._Time

-- | How long, in seconds, between the starting and ending times of the batch
-- build\'s phase.
buildBatchPhase_durationInSeconds :: Lens.Lens' BuildBatchPhase (Prelude.Maybe Prelude.Integer)
buildBatchPhase_durationInSeconds = Lens.lens (\BuildBatchPhase' {durationInSeconds} -> durationInSeconds) (\s@BuildBatchPhase' {} a -> s {durationInSeconds = a} :: BuildBatchPhase)

-- | The current status of the batch build phase. Valid values include:
--
-- [FAILED]
--     The build phase failed.
--
-- [FAULT]
--     The build phase faulted.
--
-- [IN_PROGRESS]
--     The build phase is still in progress.
--
-- [QUEUED]
--     The build has been submitted and is queued behind other submitted
--     builds.
--
-- [STOPPED]
--     The build phase stopped.
--
-- [SUCCEEDED]
--     The build phase succeeded.
--
-- [TIMED_OUT]
--     The build phase timed out.
buildBatchPhase_phaseStatus :: Lens.Lens' BuildBatchPhase (Prelude.Maybe StatusType)
buildBatchPhase_phaseStatus = Lens.lens (\BuildBatchPhase' {phaseStatus} -> phaseStatus) (\s@BuildBatchPhase' {} a -> s {phaseStatus = a} :: BuildBatchPhase)

instance Prelude.FromJSON BuildBatchPhase where
  parseJSON =
    Prelude.withObject
      "BuildBatchPhase"
      ( \x ->
          BuildBatchPhase'
            Prelude.<$> (x Prelude..:? "phaseType")
            Prelude.<*> (x Prelude..:? "contexts" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "startTime")
            Prelude.<*> (x Prelude..:? "endTime")
            Prelude.<*> (x Prelude..:? "durationInSeconds")
            Prelude.<*> (x Prelude..:? "phaseStatus")
      )

instance Prelude.Hashable BuildBatchPhase

instance Prelude.NFData BuildBatchPhase

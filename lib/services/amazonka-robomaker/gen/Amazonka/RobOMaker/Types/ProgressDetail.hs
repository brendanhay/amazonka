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
-- Module      : Amazonka.RobOMaker.Types.ProgressDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.ProgressDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.RobotDeploymentStep

-- | Information about the progress of a deployment job.
--
-- /See:/ 'newProgressDetail' smart constructor.
data ProgressDetail = ProgressDetail'
  { -- | The current progress status.
    --
    -- [Validating]
    --     Validating the deployment.
    --
    -- [DownloadingExtracting]
    --     Downloading and extracting the bundle on the robot.
    --
    -- [ExecutingPreLaunch]
    --     Executing pre-launch script(s) if provided.
    --
    -- [Launching]
    --     Launching the robot application.
    --
    -- [ExecutingPostLaunch]
    --     Executing post-launch script(s) if provided.
    --
    -- [Finished]
    --     Deployment is complete.
    currentProgress :: Prelude.Maybe RobotDeploymentStep,
    -- | Estimated amount of time in seconds remaining in the step. This
    -- currently only applies to the @Downloading\/Extracting@ step of the
    -- deployment. It is empty for other steps.
    estimatedTimeRemainingSeconds :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the deployment job.
    targetResource :: Prelude.Maybe Prelude.Text,
    -- | Precentage of the step that is done. This currently only applies to the
    -- @Downloading\/Extracting@ step of the deployment. It is empty for other
    -- steps.
    percentDone :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProgressDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentProgress', 'progressDetail_currentProgress' - The current progress status.
--
-- [Validating]
--     Validating the deployment.
--
-- [DownloadingExtracting]
--     Downloading and extracting the bundle on the robot.
--
-- [ExecutingPreLaunch]
--     Executing pre-launch script(s) if provided.
--
-- [Launching]
--     Launching the robot application.
--
-- [ExecutingPostLaunch]
--     Executing post-launch script(s) if provided.
--
-- [Finished]
--     Deployment is complete.
--
-- 'estimatedTimeRemainingSeconds', 'progressDetail_estimatedTimeRemainingSeconds' - Estimated amount of time in seconds remaining in the step. This
-- currently only applies to the @Downloading\/Extracting@ step of the
-- deployment. It is empty for other steps.
--
-- 'targetResource', 'progressDetail_targetResource' - The Amazon Resource Name (ARN) of the deployment job.
--
-- 'percentDone', 'progressDetail_percentDone' - Precentage of the step that is done. This currently only applies to the
-- @Downloading\/Extracting@ step of the deployment. It is empty for other
-- steps.
newProgressDetail ::
  ProgressDetail
newProgressDetail =
  ProgressDetail'
    { currentProgress = Prelude.Nothing,
      estimatedTimeRemainingSeconds = Prelude.Nothing,
      targetResource = Prelude.Nothing,
      percentDone = Prelude.Nothing
    }

-- | The current progress status.
--
-- [Validating]
--     Validating the deployment.
--
-- [DownloadingExtracting]
--     Downloading and extracting the bundle on the robot.
--
-- [ExecutingPreLaunch]
--     Executing pre-launch script(s) if provided.
--
-- [Launching]
--     Launching the robot application.
--
-- [ExecutingPostLaunch]
--     Executing post-launch script(s) if provided.
--
-- [Finished]
--     Deployment is complete.
progressDetail_currentProgress :: Lens.Lens' ProgressDetail (Prelude.Maybe RobotDeploymentStep)
progressDetail_currentProgress = Lens.lens (\ProgressDetail' {currentProgress} -> currentProgress) (\s@ProgressDetail' {} a -> s {currentProgress = a} :: ProgressDetail)

-- | Estimated amount of time in seconds remaining in the step. This
-- currently only applies to the @Downloading\/Extracting@ step of the
-- deployment. It is empty for other steps.
progressDetail_estimatedTimeRemainingSeconds :: Lens.Lens' ProgressDetail (Prelude.Maybe Prelude.Int)
progressDetail_estimatedTimeRemainingSeconds = Lens.lens (\ProgressDetail' {estimatedTimeRemainingSeconds} -> estimatedTimeRemainingSeconds) (\s@ProgressDetail' {} a -> s {estimatedTimeRemainingSeconds = a} :: ProgressDetail)

-- | The Amazon Resource Name (ARN) of the deployment job.
progressDetail_targetResource :: Lens.Lens' ProgressDetail (Prelude.Maybe Prelude.Text)
progressDetail_targetResource = Lens.lens (\ProgressDetail' {targetResource} -> targetResource) (\s@ProgressDetail' {} a -> s {targetResource = a} :: ProgressDetail)

-- | Precentage of the step that is done. This currently only applies to the
-- @Downloading\/Extracting@ step of the deployment. It is empty for other
-- steps.
progressDetail_percentDone :: Lens.Lens' ProgressDetail (Prelude.Maybe Prelude.Double)
progressDetail_percentDone = Lens.lens (\ProgressDetail' {percentDone} -> percentDone) (\s@ProgressDetail' {} a -> s {percentDone = a} :: ProgressDetail)

instance Core.FromJSON ProgressDetail where
  parseJSON =
    Core.withObject
      "ProgressDetail"
      ( \x ->
          ProgressDetail'
            Prelude.<$> (x Core..:? "currentProgress")
            Prelude.<*> (x Core..:? "estimatedTimeRemainingSeconds")
            Prelude.<*> (x Core..:? "targetResource")
            Prelude.<*> (x Core..:? "percentDone")
      )

instance Prelude.Hashable ProgressDetail where
  hashWithSalt salt' ProgressDetail' {..} =
    salt' `Prelude.hashWithSalt` percentDone
      `Prelude.hashWithSalt` targetResource
      `Prelude.hashWithSalt` estimatedTimeRemainingSeconds
      `Prelude.hashWithSalt` currentProgress

instance Prelude.NFData ProgressDetail where
  rnf ProgressDetail' {..} =
    Prelude.rnf currentProgress
      `Prelude.seq` Prelude.rnf percentDone
      `Prelude.seq` Prelude.rnf targetResource
      `Prelude.seq` Prelude.rnf estimatedTimeRemainingSeconds

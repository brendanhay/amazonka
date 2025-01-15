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
-- Module      : Amazonka.SageMaker.Types.EdgeDeploymentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EdgeDeploymentStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.StageStatus

-- | Contains information summarizing the deployment stage results.
--
-- /See:/ 'newEdgeDeploymentStatus' smart constructor.
data EdgeDeploymentStatus = EdgeDeploymentStatus'
  { -- | The time when the deployment API started.
    edgeDeploymentStageStartTime :: Prelude.Maybe Data.POSIX,
    -- | A detailed message about deployment status in current stage.
    edgeDeploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The general status of the current stage.
    stageStatus :: StageStatus,
    -- | The number of edge devices with the successful deployment in the current
    -- stage.
    edgeDeploymentSuccessInStage :: Prelude.Int,
    -- | The number of edge devices yet to pick up the deployment in current
    -- stage, or in progress.
    edgeDeploymentPendingInStage :: Prelude.Int,
    -- | The number of edge devices that failed the deployment in current stage.
    edgeDeploymentFailedInStage :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeDeploymentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edgeDeploymentStageStartTime', 'edgeDeploymentStatus_edgeDeploymentStageStartTime' - The time when the deployment API started.
--
-- 'edgeDeploymentStatusMessage', 'edgeDeploymentStatus_edgeDeploymentStatusMessage' - A detailed message about deployment status in current stage.
--
-- 'stageStatus', 'edgeDeploymentStatus_stageStatus' - The general status of the current stage.
--
-- 'edgeDeploymentSuccessInStage', 'edgeDeploymentStatus_edgeDeploymentSuccessInStage' - The number of edge devices with the successful deployment in the current
-- stage.
--
-- 'edgeDeploymentPendingInStage', 'edgeDeploymentStatus_edgeDeploymentPendingInStage' - The number of edge devices yet to pick up the deployment in current
-- stage, or in progress.
--
-- 'edgeDeploymentFailedInStage', 'edgeDeploymentStatus_edgeDeploymentFailedInStage' - The number of edge devices that failed the deployment in current stage.
newEdgeDeploymentStatus ::
  -- | 'stageStatus'
  StageStatus ->
  -- | 'edgeDeploymentSuccessInStage'
  Prelude.Int ->
  -- | 'edgeDeploymentPendingInStage'
  Prelude.Int ->
  -- | 'edgeDeploymentFailedInStage'
  Prelude.Int ->
  EdgeDeploymentStatus
newEdgeDeploymentStatus
  pStageStatus_
  pEdgeDeploymentSuccessInStage_
  pEdgeDeploymentPendingInStage_
  pEdgeDeploymentFailedInStage_ =
    EdgeDeploymentStatus'
      { edgeDeploymentStageStartTime =
          Prelude.Nothing,
        edgeDeploymentStatusMessage = Prelude.Nothing,
        stageStatus = pStageStatus_,
        edgeDeploymentSuccessInStage =
          pEdgeDeploymentSuccessInStage_,
        edgeDeploymentPendingInStage =
          pEdgeDeploymentPendingInStage_,
        edgeDeploymentFailedInStage =
          pEdgeDeploymentFailedInStage_
      }

-- | The time when the deployment API started.
edgeDeploymentStatus_edgeDeploymentStageStartTime :: Lens.Lens' EdgeDeploymentStatus (Prelude.Maybe Prelude.UTCTime)
edgeDeploymentStatus_edgeDeploymentStageStartTime = Lens.lens (\EdgeDeploymentStatus' {edgeDeploymentStageStartTime} -> edgeDeploymentStageStartTime) (\s@EdgeDeploymentStatus' {} a -> s {edgeDeploymentStageStartTime = a} :: EdgeDeploymentStatus) Prelude.. Lens.mapping Data._Time

-- | A detailed message about deployment status in current stage.
edgeDeploymentStatus_edgeDeploymentStatusMessage :: Lens.Lens' EdgeDeploymentStatus (Prelude.Maybe Prelude.Text)
edgeDeploymentStatus_edgeDeploymentStatusMessage = Lens.lens (\EdgeDeploymentStatus' {edgeDeploymentStatusMessage} -> edgeDeploymentStatusMessage) (\s@EdgeDeploymentStatus' {} a -> s {edgeDeploymentStatusMessage = a} :: EdgeDeploymentStatus)

-- | The general status of the current stage.
edgeDeploymentStatus_stageStatus :: Lens.Lens' EdgeDeploymentStatus StageStatus
edgeDeploymentStatus_stageStatus = Lens.lens (\EdgeDeploymentStatus' {stageStatus} -> stageStatus) (\s@EdgeDeploymentStatus' {} a -> s {stageStatus = a} :: EdgeDeploymentStatus)

-- | The number of edge devices with the successful deployment in the current
-- stage.
edgeDeploymentStatus_edgeDeploymentSuccessInStage :: Lens.Lens' EdgeDeploymentStatus Prelude.Int
edgeDeploymentStatus_edgeDeploymentSuccessInStage = Lens.lens (\EdgeDeploymentStatus' {edgeDeploymentSuccessInStage} -> edgeDeploymentSuccessInStage) (\s@EdgeDeploymentStatus' {} a -> s {edgeDeploymentSuccessInStage = a} :: EdgeDeploymentStatus)

-- | The number of edge devices yet to pick up the deployment in current
-- stage, or in progress.
edgeDeploymentStatus_edgeDeploymentPendingInStage :: Lens.Lens' EdgeDeploymentStatus Prelude.Int
edgeDeploymentStatus_edgeDeploymentPendingInStage = Lens.lens (\EdgeDeploymentStatus' {edgeDeploymentPendingInStage} -> edgeDeploymentPendingInStage) (\s@EdgeDeploymentStatus' {} a -> s {edgeDeploymentPendingInStage = a} :: EdgeDeploymentStatus)

-- | The number of edge devices that failed the deployment in current stage.
edgeDeploymentStatus_edgeDeploymentFailedInStage :: Lens.Lens' EdgeDeploymentStatus Prelude.Int
edgeDeploymentStatus_edgeDeploymentFailedInStage = Lens.lens (\EdgeDeploymentStatus' {edgeDeploymentFailedInStage} -> edgeDeploymentFailedInStage) (\s@EdgeDeploymentStatus' {} a -> s {edgeDeploymentFailedInStage = a} :: EdgeDeploymentStatus)

instance Data.FromJSON EdgeDeploymentStatus where
  parseJSON =
    Data.withObject
      "EdgeDeploymentStatus"
      ( \x ->
          EdgeDeploymentStatus'
            Prelude.<$> (x Data..:? "EdgeDeploymentStageStartTime")
            Prelude.<*> (x Data..:? "EdgeDeploymentStatusMessage")
            Prelude.<*> (x Data..: "StageStatus")
            Prelude.<*> (x Data..: "EdgeDeploymentSuccessInStage")
            Prelude.<*> (x Data..: "EdgeDeploymentPendingInStage")
            Prelude.<*> (x Data..: "EdgeDeploymentFailedInStage")
      )

instance Prelude.Hashable EdgeDeploymentStatus where
  hashWithSalt _salt EdgeDeploymentStatus' {..} =
    _salt
      `Prelude.hashWithSalt` edgeDeploymentStageStartTime
      `Prelude.hashWithSalt` edgeDeploymentStatusMessage
      `Prelude.hashWithSalt` stageStatus
      `Prelude.hashWithSalt` edgeDeploymentSuccessInStage
      `Prelude.hashWithSalt` edgeDeploymentPendingInStage
      `Prelude.hashWithSalt` edgeDeploymentFailedInStage

instance Prelude.NFData EdgeDeploymentStatus where
  rnf EdgeDeploymentStatus' {..} =
    Prelude.rnf edgeDeploymentStageStartTime `Prelude.seq`
      Prelude.rnf edgeDeploymentStatusMessage `Prelude.seq`
        Prelude.rnf stageStatus `Prelude.seq`
          Prelude.rnf edgeDeploymentSuccessInStage `Prelude.seq`
            Prelude.rnf edgeDeploymentPendingInStage `Prelude.seq`
              Prelude.rnf edgeDeploymentFailedInStage

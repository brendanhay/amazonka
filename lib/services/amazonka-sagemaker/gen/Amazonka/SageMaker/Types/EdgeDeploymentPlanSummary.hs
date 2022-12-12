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
-- Module      : Amazonka.SageMaker.Types.EdgeDeploymentPlanSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EdgeDeploymentPlanSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information summarizing an edge deployment plan.
--
-- /See:/ 'newEdgeDeploymentPlanSummary' smart constructor.
data EdgeDeploymentPlanSummary = EdgeDeploymentPlanSummary'
  { -- | The time when the edge deployment plan was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The time when the edge deployment plan was last updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the edge deployment plan.
    edgeDeploymentPlanArn :: Prelude.Text,
    -- | The name of the edge deployment plan.
    edgeDeploymentPlanName :: Prelude.Text,
    -- | The name of the device fleet used for the deployment.
    deviceFleetName :: Prelude.Text,
    -- | The number of edge devices with the successful deployment.
    edgeDeploymentSuccess :: Prelude.Int,
    -- | The number of edge devices yet to pick up the deployment, or in
    -- progress.
    edgeDeploymentPending :: Prelude.Int,
    -- | The number of edge devices that failed the deployment.
    edgeDeploymentFailed :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeDeploymentPlanSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'edgeDeploymentPlanSummary_creationTime' - The time when the edge deployment plan was created.
--
-- 'lastModifiedTime', 'edgeDeploymentPlanSummary_lastModifiedTime' - The time when the edge deployment plan was last updated.
--
-- 'edgeDeploymentPlanArn', 'edgeDeploymentPlanSummary_edgeDeploymentPlanArn' - The ARN of the edge deployment plan.
--
-- 'edgeDeploymentPlanName', 'edgeDeploymentPlanSummary_edgeDeploymentPlanName' - The name of the edge deployment plan.
--
-- 'deviceFleetName', 'edgeDeploymentPlanSummary_deviceFleetName' - The name of the device fleet used for the deployment.
--
-- 'edgeDeploymentSuccess', 'edgeDeploymentPlanSummary_edgeDeploymentSuccess' - The number of edge devices with the successful deployment.
--
-- 'edgeDeploymentPending', 'edgeDeploymentPlanSummary_edgeDeploymentPending' - The number of edge devices yet to pick up the deployment, or in
-- progress.
--
-- 'edgeDeploymentFailed', 'edgeDeploymentPlanSummary_edgeDeploymentFailed' - The number of edge devices that failed the deployment.
newEdgeDeploymentPlanSummary ::
  -- | 'edgeDeploymentPlanArn'
  Prelude.Text ->
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  -- | 'deviceFleetName'
  Prelude.Text ->
  -- | 'edgeDeploymentSuccess'
  Prelude.Int ->
  -- | 'edgeDeploymentPending'
  Prelude.Int ->
  -- | 'edgeDeploymentFailed'
  Prelude.Int ->
  EdgeDeploymentPlanSummary
newEdgeDeploymentPlanSummary
  pEdgeDeploymentPlanArn_
  pEdgeDeploymentPlanName_
  pDeviceFleetName_
  pEdgeDeploymentSuccess_
  pEdgeDeploymentPending_
  pEdgeDeploymentFailed_ =
    EdgeDeploymentPlanSummary'
      { creationTime =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        edgeDeploymentPlanArn = pEdgeDeploymentPlanArn_,
        edgeDeploymentPlanName =
          pEdgeDeploymentPlanName_,
        deviceFleetName = pDeviceFleetName_,
        edgeDeploymentSuccess = pEdgeDeploymentSuccess_,
        edgeDeploymentPending = pEdgeDeploymentPending_,
        edgeDeploymentFailed = pEdgeDeploymentFailed_
      }

-- | The time when the edge deployment plan was created.
edgeDeploymentPlanSummary_creationTime :: Lens.Lens' EdgeDeploymentPlanSummary (Prelude.Maybe Prelude.UTCTime)
edgeDeploymentPlanSummary_creationTime = Lens.lens (\EdgeDeploymentPlanSummary' {creationTime} -> creationTime) (\s@EdgeDeploymentPlanSummary' {} a -> s {creationTime = a} :: EdgeDeploymentPlanSummary) Prelude.. Lens.mapping Data._Time

-- | The time when the edge deployment plan was last updated.
edgeDeploymentPlanSummary_lastModifiedTime :: Lens.Lens' EdgeDeploymentPlanSummary (Prelude.Maybe Prelude.UTCTime)
edgeDeploymentPlanSummary_lastModifiedTime = Lens.lens (\EdgeDeploymentPlanSummary' {lastModifiedTime} -> lastModifiedTime) (\s@EdgeDeploymentPlanSummary' {} a -> s {lastModifiedTime = a} :: EdgeDeploymentPlanSummary) Prelude.. Lens.mapping Data._Time

-- | The ARN of the edge deployment plan.
edgeDeploymentPlanSummary_edgeDeploymentPlanArn :: Lens.Lens' EdgeDeploymentPlanSummary Prelude.Text
edgeDeploymentPlanSummary_edgeDeploymentPlanArn = Lens.lens (\EdgeDeploymentPlanSummary' {edgeDeploymentPlanArn} -> edgeDeploymentPlanArn) (\s@EdgeDeploymentPlanSummary' {} a -> s {edgeDeploymentPlanArn = a} :: EdgeDeploymentPlanSummary)

-- | The name of the edge deployment plan.
edgeDeploymentPlanSummary_edgeDeploymentPlanName :: Lens.Lens' EdgeDeploymentPlanSummary Prelude.Text
edgeDeploymentPlanSummary_edgeDeploymentPlanName = Lens.lens (\EdgeDeploymentPlanSummary' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@EdgeDeploymentPlanSummary' {} a -> s {edgeDeploymentPlanName = a} :: EdgeDeploymentPlanSummary)

-- | The name of the device fleet used for the deployment.
edgeDeploymentPlanSummary_deviceFleetName :: Lens.Lens' EdgeDeploymentPlanSummary Prelude.Text
edgeDeploymentPlanSummary_deviceFleetName = Lens.lens (\EdgeDeploymentPlanSummary' {deviceFleetName} -> deviceFleetName) (\s@EdgeDeploymentPlanSummary' {} a -> s {deviceFleetName = a} :: EdgeDeploymentPlanSummary)

-- | The number of edge devices with the successful deployment.
edgeDeploymentPlanSummary_edgeDeploymentSuccess :: Lens.Lens' EdgeDeploymentPlanSummary Prelude.Int
edgeDeploymentPlanSummary_edgeDeploymentSuccess = Lens.lens (\EdgeDeploymentPlanSummary' {edgeDeploymentSuccess} -> edgeDeploymentSuccess) (\s@EdgeDeploymentPlanSummary' {} a -> s {edgeDeploymentSuccess = a} :: EdgeDeploymentPlanSummary)

-- | The number of edge devices yet to pick up the deployment, or in
-- progress.
edgeDeploymentPlanSummary_edgeDeploymentPending :: Lens.Lens' EdgeDeploymentPlanSummary Prelude.Int
edgeDeploymentPlanSummary_edgeDeploymentPending = Lens.lens (\EdgeDeploymentPlanSummary' {edgeDeploymentPending} -> edgeDeploymentPending) (\s@EdgeDeploymentPlanSummary' {} a -> s {edgeDeploymentPending = a} :: EdgeDeploymentPlanSummary)

-- | The number of edge devices that failed the deployment.
edgeDeploymentPlanSummary_edgeDeploymentFailed :: Lens.Lens' EdgeDeploymentPlanSummary Prelude.Int
edgeDeploymentPlanSummary_edgeDeploymentFailed = Lens.lens (\EdgeDeploymentPlanSummary' {edgeDeploymentFailed} -> edgeDeploymentFailed) (\s@EdgeDeploymentPlanSummary' {} a -> s {edgeDeploymentFailed = a} :: EdgeDeploymentPlanSummary)

instance Data.FromJSON EdgeDeploymentPlanSummary where
  parseJSON =
    Data.withObject
      "EdgeDeploymentPlanSummary"
      ( \x ->
          EdgeDeploymentPlanSummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..: "EdgeDeploymentPlanArn")
            Prelude.<*> (x Data..: "EdgeDeploymentPlanName")
            Prelude.<*> (x Data..: "DeviceFleetName")
            Prelude.<*> (x Data..: "EdgeDeploymentSuccess")
            Prelude.<*> (x Data..: "EdgeDeploymentPending")
            Prelude.<*> (x Data..: "EdgeDeploymentFailed")
      )

instance Prelude.Hashable EdgeDeploymentPlanSummary where
  hashWithSalt _salt EdgeDeploymentPlanSummary' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` edgeDeploymentPlanArn
      `Prelude.hashWithSalt` edgeDeploymentPlanName
      `Prelude.hashWithSalt` deviceFleetName
      `Prelude.hashWithSalt` edgeDeploymentSuccess
      `Prelude.hashWithSalt` edgeDeploymentPending
      `Prelude.hashWithSalt` edgeDeploymentFailed

instance Prelude.NFData EdgeDeploymentPlanSummary where
  rnf EdgeDeploymentPlanSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanArn
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanName
      `Prelude.seq` Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf edgeDeploymentSuccess
      `Prelude.seq` Prelude.rnf edgeDeploymentPending
      `Prelude.seq` Prelude.rnf edgeDeploymentFailed

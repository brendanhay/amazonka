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
-- Module      : Amazonka.SageMaker.Types.DeviceDeploymentSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DeviceDeploymentSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DeviceDeploymentStatus

-- | Contains information summarizing device details and deployment status.
--
-- /See:/ 'newDeviceDeploymentSummary' smart constructor.
data DeviceDeploymentSummary = DeviceDeploymentSummary'
  { -- | The deployment status of the device.
    deviceDeploymentStatus :: Prelude.Maybe DeviceDeploymentStatus,
    -- | The detailed error message for the deployoment status result.
    deviceDeploymentStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the deployed stage.
    deployedStageName :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet to which the device belongs to.
    deviceFleetName :: Prelude.Maybe Prelude.Text,
    -- | The description of the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time when the deployment on the device started.
    deploymentStartTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the edge deployment plan.
    edgeDeploymentPlanArn :: Prelude.Text,
    -- | The name of the edge deployment plan.
    edgeDeploymentPlanName :: Prelude.Text,
    -- | The name of the stage in the edge deployment plan.
    stageName :: Prelude.Text,
    -- | The name of the device.
    deviceName :: Prelude.Text,
    -- | The ARN of the device.
    deviceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceDeploymentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceDeploymentStatus', 'deviceDeploymentSummary_deviceDeploymentStatus' - The deployment status of the device.
--
-- 'deviceDeploymentStatusMessage', 'deviceDeploymentSummary_deviceDeploymentStatusMessage' - The detailed error message for the deployoment status result.
--
-- 'deployedStageName', 'deviceDeploymentSummary_deployedStageName' - The name of the deployed stage.
--
-- 'deviceFleetName', 'deviceDeploymentSummary_deviceFleetName' - The name of the fleet to which the device belongs to.
--
-- 'description', 'deviceDeploymentSummary_description' - The description of the device.
--
-- 'deploymentStartTime', 'deviceDeploymentSummary_deploymentStartTime' - The time when the deployment on the device started.
--
-- 'edgeDeploymentPlanArn', 'deviceDeploymentSummary_edgeDeploymentPlanArn' - The ARN of the edge deployment plan.
--
-- 'edgeDeploymentPlanName', 'deviceDeploymentSummary_edgeDeploymentPlanName' - The name of the edge deployment plan.
--
-- 'stageName', 'deviceDeploymentSummary_stageName' - The name of the stage in the edge deployment plan.
--
-- 'deviceName', 'deviceDeploymentSummary_deviceName' - The name of the device.
--
-- 'deviceArn', 'deviceDeploymentSummary_deviceArn' - The ARN of the device.
newDeviceDeploymentSummary ::
  -- | 'edgeDeploymentPlanArn'
  Prelude.Text ->
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  -- | 'deviceName'
  Prelude.Text ->
  -- | 'deviceArn'
  Prelude.Text ->
  DeviceDeploymentSummary
newDeviceDeploymentSummary
  pEdgeDeploymentPlanArn_
  pEdgeDeploymentPlanName_
  pStageName_
  pDeviceName_
  pDeviceArn_ =
    DeviceDeploymentSummary'
      { deviceDeploymentStatus =
          Prelude.Nothing,
        deviceDeploymentStatusMessage = Prelude.Nothing,
        deployedStageName = Prelude.Nothing,
        deviceFleetName = Prelude.Nothing,
        description = Prelude.Nothing,
        deploymentStartTime = Prelude.Nothing,
        edgeDeploymentPlanArn = pEdgeDeploymentPlanArn_,
        edgeDeploymentPlanName = pEdgeDeploymentPlanName_,
        stageName = pStageName_,
        deviceName = pDeviceName_,
        deviceArn = pDeviceArn_
      }

-- | The deployment status of the device.
deviceDeploymentSummary_deviceDeploymentStatus :: Lens.Lens' DeviceDeploymentSummary (Prelude.Maybe DeviceDeploymentStatus)
deviceDeploymentSummary_deviceDeploymentStatus = Lens.lens (\DeviceDeploymentSummary' {deviceDeploymentStatus} -> deviceDeploymentStatus) (\s@DeviceDeploymentSummary' {} a -> s {deviceDeploymentStatus = a} :: DeviceDeploymentSummary)

-- | The detailed error message for the deployoment status result.
deviceDeploymentSummary_deviceDeploymentStatusMessage :: Lens.Lens' DeviceDeploymentSummary (Prelude.Maybe Prelude.Text)
deviceDeploymentSummary_deviceDeploymentStatusMessage = Lens.lens (\DeviceDeploymentSummary' {deviceDeploymentStatusMessage} -> deviceDeploymentStatusMessage) (\s@DeviceDeploymentSummary' {} a -> s {deviceDeploymentStatusMessage = a} :: DeviceDeploymentSummary)

-- | The name of the deployed stage.
deviceDeploymentSummary_deployedStageName :: Lens.Lens' DeviceDeploymentSummary (Prelude.Maybe Prelude.Text)
deviceDeploymentSummary_deployedStageName = Lens.lens (\DeviceDeploymentSummary' {deployedStageName} -> deployedStageName) (\s@DeviceDeploymentSummary' {} a -> s {deployedStageName = a} :: DeviceDeploymentSummary)

-- | The name of the fleet to which the device belongs to.
deviceDeploymentSummary_deviceFleetName :: Lens.Lens' DeviceDeploymentSummary (Prelude.Maybe Prelude.Text)
deviceDeploymentSummary_deviceFleetName = Lens.lens (\DeviceDeploymentSummary' {deviceFleetName} -> deviceFleetName) (\s@DeviceDeploymentSummary' {} a -> s {deviceFleetName = a} :: DeviceDeploymentSummary)

-- | The description of the device.
deviceDeploymentSummary_description :: Lens.Lens' DeviceDeploymentSummary (Prelude.Maybe Prelude.Text)
deviceDeploymentSummary_description = Lens.lens (\DeviceDeploymentSummary' {description} -> description) (\s@DeviceDeploymentSummary' {} a -> s {description = a} :: DeviceDeploymentSummary)

-- | The time when the deployment on the device started.
deviceDeploymentSummary_deploymentStartTime :: Lens.Lens' DeviceDeploymentSummary (Prelude.Maybe Prelude.UTCTime)
deviceDeploymentSummary_deploymentStartTime = Lens.lens (\DeviceDeploymentSummary' {deploymentStartTime} -> deploymentStartTime) (\s@DeviceDeploymentSummary' {} a -> s {deploymentStartTime = a} :: DeviceDeploymentSummary) Prelude.. Lens.mapping Data._Time

-- | The ARN of the edge deployment plan.
deviceDeploymentSummary_edgeDeploymentPlanArn :: Lens.Lens' DeviceDeploymentSummary Prelude.Text
deviceDeploymentSummary_edgeDeploymentPlanArn = Lens.lens (\DeviceDeploymentSummary' {edgeDeploymentPlanArn} -> edgeDeploymentPlanArn) (\s@DeviceDeploymentSummary' {} a -> s {edgeDeploymentPlanArn = a} :: DeviceDeploymentSummary)

-- | The name of the edge deployment plan.
deviceDeploymentSummary_edgeDeploymentPlanName :: Lens.Lens' DeviceDeploymentSummary Prelude.Text
deviceDeploymentSummary_edgeDeploymentPlanName = Lens.lens (\DeviceDeploymentSummary' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@DeviceDeploymentSummary' {} a -> s {edgeDeploymentPlanName = a} :: DeviceDeploymentSummary)

-- | The name of the stage in the edge deployment plan.
deviceDeploymentSummary_stageName :: Lens.Lens' DeviceDeploymentSummary Prelude.Text
deviceDeploymentSummary_stageName = Lens.lens (\DeviceDeploymentSummary' {stageName} -> stageName) (\s@DeviceDeploymentSummary' {} a -> s {stageName = a} :: DeviceDeploymentSummary)

-- | The name of the device.
deviceDeploymentSummary_deviceName :: Lens.Lens' DeviceDeploymentSummary Prelude.Text
deviceDeploymentSummary_deviceName = Lens.lens (\DeviceDeploymentSummary' {deviceName} -> deviceName) (\s@DeviceDeploymentSummary' {} a -> s {deviceName = a} :: DeviceDeploymentSummary)

-- | The ARN of the device.
deviceDeploymentSummary_deviceArn :: Lens.Lens' DeviceDeploymentSummary Prelude.Text
deviceDeploymentSummary_deviceArn = Lens.lens (\DeviceDeploymentSummary' {deviceArn} -> deviceArn) (\s@DeviceDeploymentSummary' {} a -> s {deviceArn = a} :: DeviceDeploymentSummary)

instance Data.FromJSON DeviceDeploymentSummary where
  parseJSON =
    Data.withObject
      "DeviceDeploymentSummary"
      ( \x ->
          DeviceDeploymentSummary'
            Prelude.<$> (x Data..:? "DeviceDeploymentStatus")
            Prelude.<*> (x Data..:? "DeviceDeploymentStatusMessage")
            Prelude.<*> (x Data..:? "DeployedStageName")
            Prelude.<*> (x Data..:? "DeviceFleetName")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DeploymentStartTime")
            Prelude.<*> (x Data..: "EdgeDeploymentPlanArn")
            Prelude.<*> (x Data..: "EdgeDeploymentPlanName")
            Prelude.<*> (x Data..: "StageName")
            Prelude.<*> (x Data..: "DeviceName")
            Prelude.<*> (x Data..: "DeviceArn")
      )

instance Prelude.Hashable DeviceDeploymentSummary where
  hashWithSalt _salt DeviceDeploymentSummary' {..} =
    _salt `Prelude.hashWithSalt` deviceDeploymentStatus
      `Prelude.hashWithSalt` deviceDeploymentStatusMessage
      `Prelude.hashWithSalt` deployedStageName
      `Prelude.hashWithSalt` deviceFleetName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deploymentStartTime
      `Prelude.hashWithSalt` edgeDeploymentPlanArn
      `Prelude.hashWithSalt` edgeDeploymentPlanName
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` deviceArn

instance Prelude.NFData DeviceDeploymentSummary where
  rnf DeviceDeploymentSummary' {..} =
    Prelude.rnf deviceDeploymentStatus
      `Prelude.seq` Prelude.rnf deviceDeploymentStatusMessage
      `Prelude.seq` Prelude.rnf deployedStageName
      `Prelude.seq` Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deploymentStartTime
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanArn
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanName
      `Prelude.seq` Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf deviceArn

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
-- Module      : Amazonka.GreengrassV2.Types.EffectiveDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.EffectiveDeployment where

import qualified Amazonka.Core as Core
import Amazonka.GreengrassV2.Types.EffectiveDeploymentExecutionStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a deployment job that IoT Greengrass sends to
-- a Greengrass core device.
--
-- /See:/ 'newEffectiveDeployment' smart constructor.
data EffectiveDeployment = EffectiveDeployment'
  { -- | The ID of the IoT job that applies the deployment to target devices.
    iotJobId :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the IoT job that applies the deployment to target devices.
    iotJobArn :: Prelude.Maybe Prelude.Text,
    -- | The reason code for the update, if the job was updated.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The description of the deployment job.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the deployment.
    deploymentId :: Prelude.Text,
    -- | The name of the deployment.
    deploymentName :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the target IoT thing or thing group.
    targetArn :: Prelude.Text,
    -- | The status of the deployment job on the Greengrass core device.
    coreDeviceExecutionStatus :: EffectiveDeploymentExecutionStatus,
    -- | The time at which the deployment was created, expressed in ISO 8601
    -- format.
    creationTimestamp :: Core.POSIX,
    -- | The time at which the deployment job was last modified, expressed in ISO
    -- 8601 format.
    modifiedTimestamp :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EffectiveDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iotJobId', 'effectiveDeployment_iotJobId' - The ID of the IoT job that applies the deployment to target devices.
--
-- 'iotJobArn', 'effectiveDeployment_iotJobArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT job that applies the deployment to target devices.
--
-- 'reason', 'effectiveDeployment_reason' - The reason code for the update, if the job was updated.
--
-- 'description', 'effectiveDeployment_description' - The description of the deployment job.
--
-- 'deploymentId', 'effectiveDeployment_deploymentId' - The ID of the deployment.
--
-- 'deploymentName', 'effectiveDeployment_deploymentName' - The name of the deployment.
--
-- 'targetArn', 'effectiveDeployment_targetArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
--
-- 'coreDeviceExecutionStatus', 'effectiveDeployment_coreDeviceExecutionStatus' - The status of the deployment job on the Greengrass core device.
--
-- 'creationTimestamp', 'effectiveDeployment_creationTimestamp' - The time at which the deployment was created, expressed in ISO 8601
-- format.
--
-- 'modifiedTimestamp', 'effectiveDeployment_modifiedTimestamp' - The time at which the deployment job was last modified, expressed in ISO
-- 8601 format.
newEffectiveDeployment ::
  -- | 'deploymentId'
  Prelude.Text ->
  -- | 'deploymentName'
  Prelude.Text ->
  -- | 'targetArn'
  Prelude.Text ->
  -- | 'coreDeviceExecutionStatus'
  EffectiveDeploymentExecutionStatus ->
  -- | 'creationTimestamp'
  Prelude.UTCTime ->
  -- | 'modifiedTimestamp'
  Prelude.UTCTime ->
  EffectiveDeployment
newEffectiveDeployment
  pDeploymentId_
  pDeploymentName_
  pTargetArn_
  pCoreDeviceExecutionStatus_
  pCreationTimestamp_
  pModifiedTimestamp_ =
    EffectiveDeployment'
      { iotJobId = Prelude.Nothing,
        iotJobArn = Prelude.Nothing,
        reason = Prelude.Nothing,
        description = Prelude.Nothing,
        deploymentId = pDeploymentId_,
        deploymentName = pDeploymentName_,
        targetArn = pTargetArn_,
        coreDeviceExecutionStatus =
          pCoreDeviceExecutionStatus_,
        creationTimestamp =
          Core._Time Lens.# pCreationTimestamp_,
        modifiedTimestamp =
          Core._Time Lens.# pModifiedTimestamp_
      }

-- | The ID of the IoT job that applies the deployment to target devices.
effectiveDeployment_iotJobId :: Lens.Lens' EffectiveDeployment (Prelude.Maybe Prelude.Text)
effectiveDeployment_iotJobId = Lens.lens (\EffectiveDeployment' {iotJobId} -> iotJobId) (\s@EffectiveDeployment' {} a -> s {iotJobId = a} :: EffectiveDeployment)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT job that applies the deployment to target devices.
effectiveDeployment_iotJobArn :: Lens.Lens' EffectiveDeployment (Prelude.Maybe Prelude.Text)
effectiveDeployment_iotJobArn = Lens.lens (\EffectiveDeployment' {iotJobArn} -> iotJobArn) (\s@EffectiveDeployment' {} a -> s {iotJobArn = a} :: EffectiveDeployment)

-- | The reason code for the update, if the job was updated.
effectiveDeployment_reason :: Lens.Lens' EffectiveDeployment (Prelude.Maybe Prelude.Text)
effectiveDeployment_reason = Lens.lens (\EffectiveDeployment' {reason} -> reason) (\s@EffectiveDeployment' {} a -> s {reason = a} :: EffectiveDeployment)

-- | The description of the deployment job.
effectiveDeployment_description :: Lens.Lens' EffectiveDeployment (Prelude.Maybe Prelude.Text)
effectiveDeployment_description = Lens.lens (\EffectiveDeployment' {description} -> description) (\s@EffectiveDeployment' {} a -> s {description = a} :: EffectiveDeployment)

-- | The ID of the deployment.
effectiveDeployment_deploymentId :: Lens.Lens' EffectiveDeployment Prelude.Text
effectiveDeployment_deploymentId = Lens.lens (\EffectiveDeployment' {deploymentId} -> deploymentId) (\s@EffectiveDeployment' {} a -> s {deploymentId = a} :: EffectiveDeployment)

-- | The name of the deployment.
effectiveDeployment_deploymentName :: Lens.Lens' EffectiveDeployment Prelude.Text
effectiveDeployment_deploymentName = Lens.lens (\EffectiveDeployment' {deploymentName} -> deploymentName) (\s@EffectiveDeployment' {} a -> s {deploymentName = a} :: EffectiveDeployment)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
effectiveDeployment_targetArn :: Lens.Lens' EffectiveDeployment Prelude.Text
effectiveDeployment_targetArn = Lens.lens (\EffectiveDeployment' {targetArn} -> targetArn) (\s@EffectiveDeployment' {} a -> s {targetArn = a} :: EffectiveDeployment)

-- | The status of the deployment job on the Greengrass core device.
effectiveDeployment_coreDeviceExecutionStatus :: Lens.Lens' EffectiveDeployment EffectiveDeploymentExecutionStatus
effectiveDeployment_coreDeviceExecutionStatus = Lens.lens (\EffectiveDeployment' {coreDeviceExecutionStatus} -> coreDeviceExecutionStatus) (\s@EffectiveDeployment' {} a -> s {coreDeviceExecutionStatus = a} :: EffectiveDeployment)

-- | The time at which the deployment was created, expressed in ISO 8601
-- format.
effectiveDeployment_creationTimestamp :: Lens.Lens' EffectiveDeployment Prelude.UTCTime
effectiveDeployment_creationTimestamp = Lens.lens (\EffectiveDeployment' {creationTimestamp} -> creationTimestamp) (\s@EffectiveDeployment' {} a -> s {creationTimestamp = a} :: EffectiveDeployment) Prelude.. Core._Time

-- | The time at which the deployment job was last modified, expressed in ISO
-- 8601 format.
effectiveDeployment_modifiedTimestamp :: Lens.Lens' EffectiveDeployment Prelude.UTCTime
effectiveDeployment_modifiedTimestamp = Lens.lens (\EffectiveDeployment' {modifiedTimestamp} -> modifiedTimestamp) (\s@EffectiveDeployment' {} a -> s {modifiedTimestamp = a} :: EffectiveDeployment) Prelude.. Core._Time

instance Core.FromJSON EffectiveDeployment where
  parseJSON =
    Core.withObject
      "EffectiveDeployment"
      ( \x ->
          EffectiveDeployment'
            Prelude.<$> (x Core..:? "iotJobId")
            Prelude.<*> (x Core..:? "iotJobArn")
            Prelude.<*> (x Core..:? "reason")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "deploymentId")
            Prelude.<*> (x Core..: "deploymentName")
            Prelude.<*> (x Core..: "targetArn")
            Prelude.<*> (x Core..: "coreDeviceExecutionStatus")
            Prelude.<*> (x Core..: "creationTimestamp")
            Prelude.<*> (x Core..: "modifiedTimestamp")
      )

instance Prelude.Hashable EffectiveDeployment

instance Prelude.NFData EffectiveDeployment

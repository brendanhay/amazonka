{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateOTAUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS IoT OTAUpdate on a target group of things or groups.
module Network.AWS.IoT.CreateOTAUpdate
  ( -- * Creating a Request
    CreateOTAUpdate (..),
    newCreateOTAUpdate,

    -- * Request Lenses
    createOTAUpdate_targetSelection,
    createOTAUpdate_awsJobTimeoutConfig,
    createOTAUpdate_protocols,
    createOTAUpdate_tags,
    createOTAUpdate_awsJobPresignedUrlConfig,
    createOTAUpdate_description,
    createOTAUpdate_additionalParameters,
    createOTAUpdate_awsJobExecutionsRolloutConfig,
    createOTAUpdate_awsJobAbortConfig,
    createOTAUpdate_otaUpdateId,
    createOTAUpdate_targets,
    createOTAUpdate_files,
    createOTAUpdate_roleArn,

    -- * Destructuring the Response
    CreateOTAUpdateResponse (..),
    newCreateOTAUpdateResponse,

    -- * Response Lenses
    createOTAUpdateResponse_otaUpdateStatus,
    createOTAUpdateResponse_otaUpdateArn,
    createOTAUpdateResponse_awsIotJobId,
    createOTAUpdateResponse_awsIotJobArn,
    createOTAUpdateResponse_otaUpdateId,
    createOTAUpdateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateOTAUpdate' smart constructor.
data CreateOTAUpdate = CreateOTAUpdate'
  { -- | Specifies whether the update will continue to run (CONTINUOUS), or will
    -- be complete after all the things specified as targets have completed the
    -- update (SNAPSHOT). If continuous, the update may also be run on a thing
    -- when a change is detected in a target. For example, an update will run
    -- on a thing when the thing is added to a target group, even after the
    -- update was completed by all things originally in the group. Valid
    -- values: CONTINUOUS | SNAPSHOT.
    targetSelection :: Core.Maybe TargetSelection,
    -- | Specifies the amount of time each device has to finish its execution of
    -- the job. A timer is started when the job execution status is set to
    -- @IN_PROGRESS@. If the job execution status is not set to another
    -- terminal state before the timer expires, it will be automatically set to
    -- @TIMED_OUT@.
    awsJobTimeoutConfig :: Core.Maybe AwsJobTimeoutConfig,
    -- | The protocol used to transfer the OTA update image. Valid values are
    -- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
    -- target device can choose the protocol.
    protocols :: Core.Maybe (Core.NonEmpty Protocol),
    -- | Metadata which can be used to manage updates.
    tags :: Core.Maybe [Tag],
    -- | Configuration information for pre-signed URLs.
    awsJobPresignedUrlConfig :: Core.Maybe AwsJobPresignedUrlConfig,
    -- | The description of the OTA update.
    description :: Core.Maybe Core.Text,
    -- | A list of additional OTA update parameters which are name-value pairs.
    additionalParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Configuration for the rollout of OTA updates.
    awsJobExecutionsRolloutConfig :: Core.Maybe AwsJobExecutionsRolloutConfig,
    -- | The criteria that determine when and how a job abort takes place.
    awsJobAbortConfig :: Core.Maybe AwsJobAbortConfig,
    -- | The ID of the OTA update to be created.
    otaUpdateId :: Core.Text,
    -- | The devices targeted to receive OTA updates.
    targets :: Core.NonEmpty Core.Text,
    -- | The files to be streamed by the OTA update.
    files :: Core.NonEmpty OTAUpdateFile,
    -- | The IAM role that grants AWS IoT access to the Amazon S3, AWS IoT jobs
    -- and AWS Code Signing resources to create an OTA update job.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOTAUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetSelection', 'createOTAUpdate_targetSelection' - Specifies whether the update will continue to run (CONTINUOUS), or will
-- be complete after all the things specified as targets have completed the
-- update (SNAPSHOT). If continuous, the update may also be run on a thing
-- when a change is detected in a target. For example, an update will run
-- on a thing when the thing is added to a target group, even after the
-- update was completed by all things originally in the group. Valid
-- values: CONTINUOUS | SNAPSHOT.
--
-- 'awsJobTimeoutConfig', 'createOTAUpdate_awsJobTimeoutConfig' - Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
--
-- 'protocols', 'createOTAUpdate_protocols' - The protocol used to transfer the OTA update image. Valid values are
-- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
-- target device can choose the protocol.
--
-- 'tags', 'createOTAUpdate_tags' - Metadata which can be used to manage updates.
--
-- 'awsJobPresignedUrlConfig', 'createOTAUpdate_awsJobPresignedUrlConfig' - Configuration information for pre-signed URLs.
--
-- 'description', 'createOTAUpdate_description' - The description of the OTA update.
--
-- 'additionalParameters', 'createOTAUpdate_additionalParameters' - A list of additional OTA update parameters which are name-value pairs.
--
-- 'awsJobExecutionsRolloutConfig', 'createOTAUpdate_awsJobExecutionsRolloutConfig' - Configuration for the rollout of OTA updates.
--
-- 'awsJobAbortConfig', 'createOTAUpdate_awsJobAbortConfig' - The criteria that determine when and how a job abort takes place.
--
-- 'otaUpdateId', 'createOTAUpdate_otaUpdateId' - The ID of the OTA update to be created.
--
-- 'targets', 'createOTAUpdate_targets' - The devices targeted to receive OTA updates.
--
-- 'files', 'createOTAUpdate_files' - The files to be streamed by the OTA update.
--
-- 'roleArn', 'createOTAUpdate_roleArn' - The IAM role that grants AWS IoT access to the Amazon S3, AWS IoT jobs
-- and AWS Code Signing resources to create an OTA update job.
newCreateOTAUpdate ::
  -- | 'otaUpdateId'
  Core.Text ->
  -- | 'targets'
  Core.NonEmpty Core.Text ->
  -- | 'files'
  Core.NonEmpty OTAUpdateFile ->
  -- | 'roleArn'
  Core.Text ->
  CreateOTAUpdate
newCreateOTAUpdate
  pOtaUpdateId_
  pTargets_
  pFiles_
  pRoleArn_ =
    CreateOTAUpdate'
      { targetSelection = Core.Nothing,
        awsJobTimeoutConfig = Core.Nothing,
        protocols = Core.Nothing,
        tags = Core.Nothing,
        awsJobPresignedUrlConfig = Core.Nothing,
        description = Core.Nothing,
        additionalParameters = Core.Nothing,
        awsJobExecutionsRolloutConfig = Core.Nothing,
        awsJobAbortConfig = Core.Nothing,
        otaUpdateId = pOtaUpdateId_,
        targets = Lens._Coerce Lens.# pTargets_,
        files = Lens._Coerce Lens.# pFiles_,
        roleArn = pRoleArn_
      }

-- | Specifies whether the update will continue to run (CONTINUOUS), or will
-- be complete after all the things specified as targets have completed the
-- update (SNAPSHOT). If continuous, the update may also be run on a thing
-- when a change is detected in a target. For example, an update will run
-- on a thing when the thing is added to a target group, even after the
-- update was completed by all things originally in the group. Valid
-- values: CONTINUOUS | SNAPSHOT.
createOTAUpdate_targetSelection :: Lens.Lens' CreateOTAUpdate (Core.Maybe TargetSelection)
createOTAUpdate_targetSelection = Lens.lens (\CreateOTAUpdate' {targetSelection} -> targetSelection) (\s@CreateOTAUpdate' {} a -> s {targetSelection = a} :: CreateOTAUpdate)

-- | Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
createOTAUpdate_awsJobTimeoutConfig :: Lens.Lens' CreateOTAUpdate (Core.Maybe AwsJobTimeoutConfig)
createOTAUpdate_awsJobTimeoutConfig = Lens.lens (\CreateOTAUpdate' {awsJobTimeoutConfig} -> awsJobTimeoutConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobTimeoutConfig = a} :: CreateOTAUpdate)

-- | The protocol used to transfer the OTA update image. Valid values are
-- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
-- target device can choose the protocol.
createOTAUpdate_protocols :: Lens.Lens' CreateOTAUpdate (Core.Maybe (Core.NonEmpty Protocol))
createOTAUpdate_protocols = Lens.lens (\CreateOTAUpdate' {protocols} -> protocols) (\s@CreateOTAUpdate' {} a -> s {protocols = a} :: CreateOTAUpdate) Core.. Lens.mapping Lens._Coerce

-- | Metadata which can be used to manage updates.
createOTAUpdate_tags :: Lens.Lens' CreateOTAUpdate (Core.Maybe [Tag])
createOTAUpdate_tags = Lens.lens (\CreateOTAUpdate' {tags} -> tags) (\s@CreateOTAUpdate' {} a -> s {tags = a} :: CreateOTAUpdate) Core.. Lens.mapping Lens._Coerce

-- | Configuration information for pre-signed URLs.
createOTAUpdate_awsJobPresignedUrlConfig :: Lens.Lens' CreateOTAUpdate (Core.Maybe AwsJobPresignedUrlConfig)
createOTAUpdate_awsJobPresignedUrlConfig = Lens.lens (\CreateOTAUpdate' {awsJobPresignedUrlConfig} -> awsJobPresignedUrlConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobPresignedUrlConfig = a} :: CreateOTAUpdate)

-- | The description of the OTA update.
createOTAUpdate_description :: Lens.Lens' CreateOTAUpdate (Core.Maybe Core.Text)
createOTAUpdate_description = Lens.lens (\CreateOTAUpdate' {description} -> description) (\s@CreateOTAUpdate' {} a -> s {description = a} :: CreateOTAUpdate)

-- | A list of additional OTA update parameters which are name-value pairs.
createOTAUpdate_additionalParameters :: Lens.Lens' CreateOTAUpdate (Core.Maybe (Core.HashMap Core.Text Core.Text))
createOTAUpdate_additionalParameters = Lens.lens (\CreateOTAUpdate' {additionalParameters} -> additionalParameters) (\s@CreateOTAUpdate' {} a -> s {additionalParameters = a} :: CreateOTAUpdate) Core.. Lens.mapping Lens._Coerce

-- | Configuration for the rollout of OTA updates.
createOTAUpdate_awsJobExecutionsRolloutConfig :: Lens.Lens' CreateOTAUpdate (Core.Maybe AwsJobExecutionsRolloutConfig)
createOTAUpdate_awsJobExecutionsRolloutConfig = Lens.lens (\CreateOTAUpdate' {awsJobExecutionsRolloutConfig} -> awsJobExecutionsRolloutConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobExecutionsRolloutConfig = a} :: CreateOTAUpdate)

-- | The criteria that determine when and how a job abort takes place.
createOTAUpdate_awsJobAbortConfig :: Lens.Lens' CreateOTAUpdate (Core.Maybe AwsJobAbortConfig)
createOTAUpdate_awsJobAbortConfig = Lens.lens (\CreateOTAUpdate' {awsJobAbortConfig} -> awsJobAbortConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobAbortConfig = a} :: CreateOTAUpdate)

-- | The ID of the OTA update to be created.
createOTAUpdate_otaUpdateId :: Lens.Lens' CreateOTAUpdate Core.Text
createOTAUpdate_otaUpdateId = Lens.lens (\CreateOTAUpdate' {otaUpdateId} -> otaUpdateId) (\s@CreateOTAUpdate' {} a -> s {otaUpdateId = a} :: CreateOTAUpdate)

-- | The devices targeted to receive OTA updates.
createOTAUpdate_targets :: Lens.Lens' CreateOTAUpdate (Core.NonEmpty Core.Text)
createOTAUpdate_targets = Lens.lens (\CreateOTAUpdate' {targets} -> targets) (\s@CreateOTAUpdate' {} a -> s {targets = a} :: CreateOTAUpdate) Core.. Lens._Coerce

-- | The files to be streamed by the OTA update.
createOTAUpdate_files :: Lens.Lens' CreateOTAUpdate (Core.NonEmpty OTAUpdateFile)
createOTAUpdate_files = Lens.lens (\CreateOTAUpdate' {files} -> files) (\s@CreateOTAUpdate' {} a -> s {files = a} :: CreateOTAUpdate) Core.. Lens._Coerce

-- | The IAM role that grants AWS IoT access to the Amazon S3, AWS IoT jobs
-- and AWS Code Signing resources to create an OTA update job.
createOTAUpdate_roleArn :: Lens.Lens' CreateOTAUpdate Core.Text
createOTAUpdate_roleArn = Lens.lens (\CreateOTAUpdate' {roleArn} -> roleArn) (\s@CreateOTAUpdate' {} a -> s {roleArn = a} :: CreateOTAUpdate)

instance Core.AWSRequest CreateOTAUpdate where
  type
    AWSResponse CreateOTAUpdate =
      CreateOTAUpdateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOTAUpdateResponse'
            Core.<$> (x Core..?> "otaUpdateStatus")
            Core.<*> (x Core..?> "otaUpdateArn")
            Core.<*> (x Core..?> "awsIotJobId")
            Core.<*> (x Core..?> "awsIotJobArn")
            Core.<*> (x Core..?> "otaUpdateId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateOTAUpdate

instance Core.NFData CreateOTAUpdate

instance Core.ToHeaders CreateOTAUpdate where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateOTAUpdate where
  toJSON CreateOTAUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("targetSelection" Core..=)
              Core.<$> targetSelection,
            ("awsJobTimeoutConfig" Core..=)
              Core.<$> awsJobTimeoutConfig,
            ("protocols" Core..=) Core.<$> protocols,
            ("tags" Core..=) Core.<$> tags,
            ("awsJobPresignedUrlConfig" Core..=)
              Core.<$> awsJobPresignedUrlConfig,
            ("description" Core..=) Core.<$> description,
            ("additionalParameters" Core..=)
              Core.<$> additionalParameters,
            ("awsJobExecutionsRolloutConfig" Core..=)
              Core.<$> awsJobExecutionsRolloutConfig,
            ("awsJobAbortConfig" Core..=)
              Core.<$> awsJobAbortConfig,
            Core.Just ("targets" Core..= targets),
            Core.Just ("files" Core..= files),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateOTAUpdate where
  toPath CreateOTAUpdate' {..} =
    Core.mconcat
      ["/otaUpdates/", Core.toBS otaUpdateId]

instance Core.ToQuery CreateOTAUpdate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateOTAUpdateResponse' smart constructor.
data CreateOTAUpdateResponse = CreateOTAUpdateResponse'
  { -- | The OTA update status.
    otaUpdateStatus :: Core.Maybe OTAUpdateStatus,
    -- | The OTA update ARN.
    otaUpdateArn :: Core.Maybe Core.Text,
    -- | The AWS IoT job ID associated with the OTA update.
    awsIotJobId :: Core.Maybe Core.Text,
    -- | The AWS IoT job ARN associated with the OTA update.
    awsIotJobArn :: Core.Maybe Core.Text,
    -- | The OTA update ID.
    otaUpdateId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOTAUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otaUpdateStatus', 'createOTAUpdateResponse_otaUpdateStatus' - The OTA update status.
--
-- 'otaUpdateArn', 'createOTAUpdateResponse_otaUpdateArn' - The OTA update ARN.
--
-- 'awsIotJobId', 'createOTAUpdateResponse_awsIotJobId' - The AWS IoT job ID associated with the OTA update.
--
-- 'awsIotJobArn', 'createOTAUpdateResponse_awsIotJobArn' - The AWS IoT job ARN associated with the OTA update.
--
-- 'otaUpdateId', 'createOTAUpdateResponse_otaUpdateId' - The OTA update ID.
--
-- 'httpStatus', 'createOTAUpdateResponse_httpStatus' - The response's http status code.
newCreateOTAUpdateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateOTAUpdateResponse
newCreateOTAUpdateResponse pHttpStatus_ =
  CreateOTAUpdateResponse'
    { otaUpdateStatus =
        Core.Nothing,
      otaUpdateArn = Core.Nothing,
      awsIotJobId = Core.Nothing,
      awsIotJobArn = Core.Nothing,
      otaUpdateId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The OTA update status.
createOTAUpdateResponse_otaUpdateStatus :: Lens.Lens' CreateOTAUpdateResponse (Core.Maybe OTAUpdateStatus)
createOTAUpdateResponse_otaUpdateStatus = Lens.lens (\CreateOTAUpdateResponse' {otaUpdateStatus} -> otaUpdateStatus) (\s@CreateOTAUpdateResponse' {} a -> s {otaUpdateStatus = a} :: CreateOTAUpdateResponse)

-- | The OTA update ARN.
createOTAUpdateResponse_otaUpdateArn :: Lens.Lens' CreateOTAUpdateResponse (Core.Maybe Core.Text)
createOTAUpdateResponse_otaUpdateArn = Lens.lens (\CreateOTAUpdateResponse' {otaUpdateArn} -> otaUpdateArn) (\s@CreateOTAUpdateResponse' {} a -> s {otaUpdateArn = a} :: CreateOTAUpdateResponse)

-- | The AWS IoT job ID associated with the OTA update.
createOTAUpdateResponse_awsIotJobId :: Lens.Lens' CreateOTAUpdateResponse (Core.Maybe Core.Text)
createOTAUpdateResponse_awsIotJobId = Lens.lens (\CreateOTAUpdateResponse' {awsIotJobId} -> awsIotJobId) (\s@CreateOTAUpdateResponse' {} a -> s {awsIotJobId = a} :: CreateOTAUpdateResponse)

-- | The AWS IoT job ARN associated with the OTA update.
createOTAUpdateResponse_awsIotJobArn :: Lens.Lens' CreateOTAUpdateResponse (Core.Maybe Core.Text)
createOTAUpdateResponse_awsIotJobArn = Lens.lens (\CreateOTAUpdateResponse' {awsIotJobArn} -> awsIotJobArn) (\s@CreateOTAUpdateResponse' {} a -> s {awsIotJobArn = a} :: CreateOTAUpdateResponse)

-- | The OTA update ID.
createOTAUpdateResponse_otaUpdateId :: Lens.Lens' CreateOTAUpdateResponse (Core.Maybe Core.Text)
createOTAUpdateResponse_otaUpdateId = Lens.lens (\CreateOTAUpdateResponse' {otaUpdateId} -> otaUpdateId) (\s@CreateOTAUpdateResponse' {} a -> s {otaUpdateId = a} :: CreateOTAUpdateResponse)

-- | The response's http status code.
createOTAUpdateResponse_httpStatus :: Lens.Lens' CreateOTAUpdateResponse Core.Int
createOTAUpdateResponse_httpStatus = Lens.lens (\CreateOTAUpdateResponse' {httpStatus} -> httpStatus) (\s@CreateOTAUpdateResponse' {} a -> s {httpStatus = a} :: CreateOTAUpdateResponse)

instance Core.NFData CreateOTAUpdateResponse

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
-- Creates an IoT OTA update on a target group of things or groups.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateOTAUpdate>
-- action.
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
    createOTAUpdateResponse_awsIotJobArn,
    createOTAUpdateResponse_awsIotJobId,
    createOTAUpdateResponse_otaUpdateId,
    createOTAUpdateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | Specifies the amount of time each device has to finish its execution of
    -- the job. A timer is started when the job execution status is set to
    -- @IN_PROGRESS@. If the job execution status is not set to another
    -- terminal state before the timer expires, it will be automatically set to
    -- @TIMED_OUT@.
    awsJobTimeoutConfig :: Prelude.Maybe AwsJobTimeoutConfig,
    -- | The protocol used to transfer the OTA update image. Valid values are
    -- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
    -- target device can choose the protocol.
    protocols :: Prelude.Maybe (Prelude.NonEmpty Protocol),
    -- | Metadata which can be used to manage updates.
    tags :: Prelude.Maybe [Tag],
    -- | Configuration information for pre-signed URLs.
    awsJobPresignedUrlConfig :: Prelude.Maybe AwsJobPresignedUrlConfig,
    -- | The description of the OTA update.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of additional OTA update parameters which are name-value pairs.
    additionalParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Configuration for the rollout of OTA updates.
    awsJobExecutionsRolloutConfig :: Prelude.Maybe AwsJobExecutionsRolloutConfig,
    -- | The criteria that determine when and how a job abort takes place.
    awsJobAbortConfig :: Prelude.Maybe AwsJobAbortConfig,
    -- | The ID of the OTA update to be created.
    otaUpdateId :: Prelude.Text,
    -- | The devices targeted to receive OTA updates.
    targets :: Prelude.NonEmpty Prelude.Text,
    -- | The files to be streamed by the OTA update.
    files :: Prelude.NonEmpty OTAUpdateFile,
    -- | The IAM role that grants Amazon Web Services IoT Core access to the
    -- Amazon S3, IoT jobs and Amazon Web Services Code Signing resources to
    -- create an OTA update job.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'roleArn', 'createOTAUpdate_roleArn' - The IAM role that grants Amazon Web Services IoT Core access to the
-- Amazon S3, IoT jobs and Amazon Web Services Code Signing resources to
-- create an OTA update job.
newCreateOTAUpdate ::
  -- | 'otaUpdateId'
  Prelude.Text ->
  -- | 'targets'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'files'
  Prelude.NonEmpty OTAUpdateFile ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateOTAUpdate
newCreateOTAUpdate
  pOtaUpdateId_
  pTargets_
  pFiles_
  pRoleArn_ =
    CreateOTAUpdate'
      { targetSelection = Prelude.Nothing,
        awsJobTimeoutConfig = Prelude.Nothing,
        protocols = Prelude.Nothing,
        tags = Prelude.Nothing,
        awsJobPresignedUrlConfig = Prelude.Nothing,
        description = Prelude.Nothing,
        additionalParameters = Prelude.Nothing,
        awsJobExecutionsRolloutConfig = Prelude.Nothing,
        awsJobAbortConfig = Prelude.Nothing,
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
createOTAUpdate_targetSelection :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe TargetSelection)
createOTAUpdate_targetSelection = Lens.lens (\CreateOTAUpdate' {targetSelection} -> targetSelection) (\s@CreateOTAUpdate' {} a -> s {targetSelection = a} :: CreateOTAUpdate)

-- | Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
createOTAUpdate_awsJobTimeoutConfig :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe AwsJobTimeoutConfig)
createOTAUpdate_awsJobTimeoutConfig = Lens.lens (\CreateOTAUpdate' {awsJobTimeoutConfig} -> awsJobTimeoutConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobTimeoutConfig = a} :: CreateOTAUpdate)

-- | The protocol used to transfer the OTA update image. Valid values are
-- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
-- target device can choose the protocol.
createOTAUpdate_protocols :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe (Prelude.NonEmpty Protocol))
createOTAUpdate_protocols = Lens.lens (\CreateOTAUpdate' {protocols} -> protocols) (\s@CreateOTAUpdate' {} a -> s {protocols = a} :: CreateOTAUpdate) Prelude.. Lens.mapping Lens._Coerce

-- | Metadata which can be used to manage updates.
createOTAUpdate_tags :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe [Tag])
createOTAUpdate_tags = Lens.lens (\CreateOTAUpdate' {tags} -> tags) (\s@CreateOTAUpdate' {} a -> s {tags = a} :: CreateOTAUpdate) Prelude.. Lens.mapping Lens._Coerce

-- | Configuration information for pre-signed URLs.
createOTAUpdate_awsJobPresignedUrlConfig :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe AwsJobPresignedUrlConfig)
createOTAUpdate_awsJobPresignedUrlConfig = Lens.lens (\CreateOTAUpdate' {awsJobPresignedUrlConfig} -> awsJobPresignedUrlConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobPresignedUrlConfig = a} :: CreateOTAUpdate)

-- | The description of the OTA update.
createOTAUpdate_description :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe Prelude.Text)
createOTAUpdate_description = Lens.lens (\CreateOTAUpdate' {description} -> description) (\s@CreateOTAUpdate' {} a -> s {description = a} :: CreateOTAUpdate)

-- | A list of additional OTA update parameters which are name-value pairs.
createOTAUpdate_additionalParameters :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createOTAUpdate_additionalParameters = Lens.lens (\CreateOTAUpdate' {additionalParameters} -> additionalParameters) (\s@CreateOTAUpdate' {} a -> s {additionalParameters = a} :: CreateOTAUpdate) Prelude.. Lens.mapping Lens._Coerce

-- | Configuration for the rollout of OTA updates.
createOTAUpdate_awsJobExecutionsRolloutConfig :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe AwsJobExecutionsRolloutConfig)
createOTAUpdate_awsJobExecutionsRolloutConfig = Lens.lens (\CreateOTAUpdate' {awsJobExecutionsRolloutConfig} -> awsJobExecutionsRolloutConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobExecutionsRolloutConfig = a} :: CreateOTAUpdate)

-- | The criteria that determine when and how a job abort takes place.
createOTAUpdate_awsJobAbortConfig :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe AwsJobAbortConfig)
createOTAUpdate_awsJobAbortConfig = Lens.lens (\CreateOTAUpdate' {awsJobAbortConfig} -> awsJobAbortConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobAbortConfig = a} :: CreateOTAUpdate)

-- | The ID of the OTA update to be created.
createOTAUpdate_otaUpdateId :: Lens.Lens' CreateOTAUpdate Prelude.Text
createOTAUpdate_otaUpdateId = Lens.lens (\CreateOTAUpdate' {otaUpdateId} -> otaUpdateId) (\s@CreateOTAUpdate' {} a -> s {otaUpdateId = a} :: CreateOTAUpdate)

-- | The devices targeted to receive OTA updates.
createOTAUpdate_targets :: Lens.Lens' CreateOTAUpdate (Prelude.NonEmpty Prelude.Text)
createOTAUpdate_targets = Lens.lens (\CreateOTAUpdate' {targets} -> targets) (\s@CreateOTAUpdate' {} a -> s {targets = a} :: CreateOTAUpdate) Prelude.. Lens._Coerce

-- | The files to be streamed by the OTA update.
createOTAUpdate_files :: Lens.Lens' CreateOTAUpdate (Prelude.NonEmpty OTAUpdateFile)
createOTAUpdate_files = Lens.lens (\CreateOTAUpdate' {files} -> files) (\s@CreateOTAUpdate' {} a -> s {files = a} :: CreateOTAUpdate) Prelude.. Lens._Coerce

-- | The IAM role that grants Amazon Web Services IoT Core access to the
-- Amazon S3, IoT jobs and Amazon Web Services Code Signing resources to
-- create an OTA update job.
createOTAUpdate_roleArn :: Lens.Lens' CreateOTAUpdate Prelude.Text
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
            Prelude.<$> (x Core..?> "otaUpdateStatus")
            Prelude.<*> (x Core..?> "otaUpdateArn")
            Prelude.<*> (x Core..?> "awsIotJobArn")
            Prelude.<*> (x Core..?> "awsIotJobId")
            Prelude.<*> (x Core..?> "otaUpdateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOTAUpdate

instance Prelude.NFData CreateOTAUpdate

instance Core.ToHeaders CreateOTAUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateOTAUpdate where
  toJSON CreateOTAUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("targetSelection" Core..=)
              Prelude.<$> targetSelection,
            ("awsJobTimeoutConfig" Core..=)
              Prelude.<$> awsJobTimeoutConfig,
            ("protocols" Core..=) Prelude.<$> protocols,
            ("tags" Core..=) Prelude.<$> tags,
            ("awsJobPresignedUrlConfig" Core..=)
              Prelude.<$> awsJobPresignedUrlConfig,
            ("description" Core..=) Prelude.<$> description,
            ("additionalParameters" Core..=)
              Prelude.<$> additionalParameters,
            ("awsJobExecutionsRolloutConfig" Core..=)
              Prelude.<$> awsJobExecutionsRolloutConfig,
            ("awsJobAbortConfig" Core..=)
              Prelude.<$> awsJobAbortConfig,
            Prelude.Just ("targets" Core..= targets),
            Prelude.Just ("files" Core..= files),
            Prelude.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateOTAUpdate where
  toPath CreateOTAUpdate' {..} =
    Prelude.mconcat
      ["/otaUpdates/", Core.toBS otaUpdateId]

instance Core.ToQuery CreateOTAUpdate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOTAUpdateResponse' smart constructor.
data CreateOTAUpdateResponse = CreateOTAUpdateResponse'
  { -- | The OTA update status.
    otaUpdateStatus :: Prelude.Maybe OTAUpdateStatus,
    -- | The OTA update ARN.
    otaUpdateArn :: Prelude.Maybe Prelude.Text,
    -- | The IoT job ARN associated with the OTA update.
    awsIotJobArn :: Prelude.Maybe Prelude.Text,
    -- | The IoT job ID associated with the OTA update.
    awsIotJobId :: Prelude.Maybe Prelude.Text,
    -- | The OTA update ID.
    otaUpdateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'awsIotJobArn', 'createOTAUpdateResponse_awsIotJobArn' - The IoT job ARN associated with the OTA update.
--
-- 'awsIotJobId', 'createOTAUpdateResponse_awsIotJobId' - The IoT job ID associated with the OTA update.
--
-- 'otaUpdateId', 'createOTAUpdateResponse_otaUpdateId' - The OTA update ID.
--
-- 'httpStatus', 'createOTAUpdateResponse_httpStatus' - The response's http status code.
newCreateOTAUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOTAUpdateResponse
newCreateOTAUpdateResponse pHttpStatus_ =
  CreateOTAUpdateResponse'
    { otaUpdateStatus =
        Prelude.Nothing,
      otaUpdateArn = Prelude.Nothing,
      awsIotJobArn = Prelude.Nothing,
      awsIotJobId = Prelude.Nothing,
      otaUpdateId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The OTA update status.
createOTAUpdateResponse_otaUpdateStatus :: Lens.Lens' CreateOTAUpdateResponse (Prelude.Maybe OTAUpdateStatus)
createOTAUpdateResponse_otaUpdateStatus = Lens.lens (\CreateOTAUpdateResponse' {otaUpdateStatus} -> otaUpdateStatus) (\s@CreateOTAUpdateResponse' {} a -> s {otaUpdateStatus = a} :: CreateOTAUpdateResponse)

-- | The OTA update ARN.
createOTAUpdateResponse_otaUpdateArn :: Lens.Lens' CreateOTAUpdateResponse (Prelude.Maybe Prelude.Text)
createOTAUpdateResponse_otaUpdateArn = Lens.lens (\CreateOTAUpdateResponse' {otaUpdateArn} -> otaUpdateArn) (\s@CreateOTAUpdateResponse' {} a -> s {otaUpdateArn = a} :: CreateOTAUpdateResponse)

-- | The IoT job ARN associated with the OTA update.
createOTAUpdateResponse_awsIotJobArn :: Lens.Lens' CreateOTAUpdateResponse (Prelude.Maybe Prelude.Text)
createOTAUpdateResponse_awsIotJobArn = Lens.lens (\CreateOTAUpdateResponse' {awsIotJobArn} -> awsIotJobArn) (\s@CreateOTAUpdateResponse' {} a -> s {awsIotJobArn = a} :: CreateOTAUpdateResponse)

-- | The IoT job ID associated with the OTA update.
createOTAUpdateResponse_awsIotJobId :: Lens.Lens' CreateOTAUpdateResponse (Prelude.Maybe Prelude.Text)
createOTAUpdateResponse_awsIotJobId = Lens.lens (\CreateOTAUpdateResponse' {awsIotJobId} -> awsIotJobId) (\s@CreateOTAUpdateResponse' {} a -> s {awsIotJobId = a} :: CreateOTAUpdateResponse)

-- | The OTA update ID.
createOTAUpdateResponse_otaUpdateId :: Lens.Lens' CreateOTAUpdateResponse (Prelude.Maybe Prelude.Text)
createOTAUpdateResponse_otaUpdateId = Lens.lens (\CreateOTAUpdateResponse' {otaUpdateId} -> otaUpdateId) (\s@CreateOTAUpdateResponse' {} a -> s {otaUpdateId = a} :: CreateOTAUpdateResponse)

-- | The response's http status code.
createOTAUpdateResponse_httpStatus :: Lens.Lens' CreateOTAUpdateResponse Prelude.Int
createOTAUpdateResponse_httpStatus = Lens.lens (\CreateOTAUpdateResponse' {httpStatus} -> httpStatus) (\s@CreateOTAUpdateResponse' {} a -> s {httpStatus = a} :: CreateOTAUpdateResponse)

instance Prelude.NFData CreateOTAUpdateResponse

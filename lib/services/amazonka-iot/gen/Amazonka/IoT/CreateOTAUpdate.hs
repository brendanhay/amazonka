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
-- Module      : Amazonka.IoT.CreateOTAUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IoT OTA update on a target group of things or groups.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateOTAUpdate>
-- action.
module Amazonka.IoT.CreateOTAUpdate
  ( -- * Creating a Request
    CreateOTAUpdate (..),
    newCreateOTAUpdate,

    -- * Request Lenses
    createOTAUpdate_additionalParameters,
    createOTAUpdate_awsJobAbortConfig,
    createOTAUpdate_awsJobExecutionsRolloutConfig,
    createOTAUpdate_awsJobPresignedUrlConfig,
    createOTAUpdate_awsJobTimeoutConfig,
    createOTAUpdate_description,
    createOTAUpdate_protocols,
    createOTAUpdate_tags,
    createOTAUpdate_targetSelection,
    createOTAUpdate_otaUpdateId,
    createOTAUpdate_targets,
    createOTAUpdate_files,
    createOTAUpdate_roleArn,

    -- * Destructuring the Response
    CreateOTAUpdateResponse (..),
    newCreateOTAUpdateResponse,

    -- * Response Lenses
    createOTAUpdateResponse_awsIotJobArn,
    createOTAUpdateResponse_awsIotJobId,
    createOTAUpdateResponse_otaUpdateArn,
    createOTAUpdateResponse_otaUpdateId,
    createOTAUpdateResponse_otaUpdateStatus,
    createOTAUpdateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateOTAUpdate' smart constructor.
data CreateOTAUpdate = CreateOTAUpdate'
  { -- | A list of additional OTA update parameters which are name-value pairs.
    additionalParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The criteria that determine when and how a job abort takes place.
    awsJobAbortConfig :: Prelude.Maybe AwsJobAbortConfig,
    -- | Configuration for the rollout of OTA updates.
    awsJobExecutionsRolloutConfig :: Prelude.Maybe AwsJobExecutionsRolloutConfig,
    -- | Configuration information for pre-signed URLs.
    awsJobPresignedUrlConfig :: Prelude.Maybe AwsJobPresignedUrlConfig,
    -- | Specifies the amount of time each device has to finish its execution of
    -- the job. A timer is started when the job execution status is set to
    -- @IN_PROGRESS@. If the job execution status is not set to another
    -- terminal state before the timer expires, it will be automatically set to
    -- @TIMED_OUT@.
    awsJobTimeoutConfig :: Prelude.Maybe AwsJobTimeoutConfig,
    -- | The description of the OTA update.
    description :: Prelude.Maybe Prelude.Text,
    -- | The protocol used to transfer the OTA update image. Valid values are
    -- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
    -- target device can choose the protocol.
    protocols :: Prelude.Maybe (Prelude.NonEmpty Protocol),
    -- | Metadata which can be used to manage updates.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies whether the update will continue to run (CONTINUOUS), or will
    -- be complete after all the things specified as targets have completed the
    -- update (SNAPSHOT). If continuous, the update may also be run on a thing
    -- when a change is detected in a target. For example, an update will run
    -- on a thing when the thing is added to a target group, even after the
    -- update was completed by all things originally in the group. Valid
    -- values: CONTINUOUS | SNAPSHOT.
    targetSelection :: Prelude.Maybe TargetSelection,
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
-- 'additionalParameters', 'createOTAUpdate_additionalParameters' - A list of additional OTA update parameters which are name-value pairs.
--
-- 'awsJobAbortConfig', 'createOTAUpdate_awsJobAbortConfig' - The criteria that determine when and how a job abort takes place.
--
-- 'awsJobExecutionsRolloutConfig', 'createOTAUpdate_awsJobExecutionsRolloutConfig' - Configuration for the rollout of OTA updates.
--
-- 'awsJobPresignedUrlConfig', 'createOTAUpdate_awsJobPresignedUrlConfig' - Configuration information for pre-signed URLs.
--
-- 'awsJobTimeoutConfig', 'createOTAUpdate_awsJobTimeoutConfig' - Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
--
-- 'description', 'createOTAUpdate_description' - The description of the OTA update.
--
-- 'protocols', 'createOTAUpdate_protocols' - The protocol used to transfer the OTA update image. Valid values are
-- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
-- target device can choose the protocol.
--
-- 'tags', 'createOTAUpdate_tags' - Metadata which can be used to manage updates.
--
-- 'targetSelection', 'createOTAUpdate_targetSelection' - Specifies whether the update will continue to run (CONTINUOUS), or will
-- be complete after all the things specified as targets have completed the
-- update (SNAPSHOT). If continuous, the update may also be run on a thing
-- when a change is detected in a target. For example, an update will run
-- on a thing when the thing is added to a target group, even after the
-- update was completed by all things originally in the group. Valid
-- values: CONTINUOUS | SNAPSHOT.
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
      { additionalParameters =
          Prelude.Nothing,
        awsJobAbortConfig = Prelude.Nothing,
        awsJobExecutionsRolloutConfig = Prelude.Nothing,
        awsJobPresignedUrlConfig = Prelude.Nothing,
        awsJobTimeoutConfig = Prelude.Nothing,
        description = Prelude.Nothing,
        protocols = Prelude.Nothing,
        tags = Prelude.Nothing,
        targetSelection = Prelude.Nothing,
        otaUpdateId = pOtaUpdateId_,
        targets = Lens.coerced Lens.# pTargets_,
        files = Lens.coerced Lens.# pFiles_,
        roleArn = pRoleArn_
      }

-- | A list of additional OTA update parameters which are name-value pairs.
createOTAUpdate_additionalParameters :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createOTAUpdate_additionalParameters = Lens.lens (\CreateOTAUpdate' {additionalParameters} -> additionalParameters) (\s@CreateOTAUpdate' {} a -> s {additionalParameters = a} :: CreateOTAUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The criteria that determine when and how a job abort takes place.
createOTAUpdate_awsJobAbortConfig :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe AwsJobAbortConfig)
createOTAUpdate_awsJobAbortConfig = Lens.lens (\CreateOTAUpdate' {awsJobAbortConfig} -> awsJobAbortConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobAbortConfig = a} :: CreateOTAUpdate)

-- | Configuration for the rollout of OTA updates.
createOTAUpdate_awsJobExecutionsRolloutConfig :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe AwsJobExecutionsRolloutConfig)
createOTAUpdate_awsJobExecutionsRolloutConfig = Lens.lens (\CreateOTAUpdate' {awsJobExecutionsRolloutConfig} -> awsJobExecutionsRolloutConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobExecutionsRolloutConfig = a} :: CreateOTAUpdate)

-- | Configuration information for pre-signed URLs.
createOTAUpdate_awsJobPresignedUrlConfig :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe AwsJobPresignedUrlConfig)
createOTAUpdate_awsJobPresignedUrlConfig = Lens.lens (\CreateOTAUpdate' {awsJobPresignedUrlConfig} -> awsJobPresignedUrlConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobPresignedUrlConfig = a} :: CreateOTAUpdate)

-- | Specifies the amount of time each device has to finish its execution of
-- the job. A timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the timer expires, it will be automatically set to
-- @TIMED_OUT@.
createOTAUpdate_awsJobTimeoutConfig :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe AwsJobTimeoutConfig)
createOTAUpdate_awsJobTimeoutConfig = Lens.lens (\CreateOTAUpdate' {awsJobTimeoutConfig} -> awsJobTimeoutConfig) (\s@CreateOTAUpdate' {} a -> s {awsJobTimeoutConfig = a} :: CreateOTAUpdate)

-- | The description of the OTA update.
createOTAUpdate_description :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe Prelude.Text)
createOTAUpdate_description = Lens.lens (\CreateOTAUpdate' {description} -> description) (\s@CreateOTAUpdate' {} a -> s {description = a} :: CreateOTAUpdate)

-- | The protocol used to transfer the OTA update image. Valid values are
-- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
-- target device can choose the protocol.
createOTAUpdate_protocols :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe (Prelude.NonEmpty Protocol))
createOTAUpdate_protocols = Lens.lens (\CreateOTAUpdate' {protocols} -> protocols) (\s@CreateOTAUpdate' {} a -> s {protocols = a} :: CreateOTAUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Metadata which can be used to manage updates.
createOTAUpdate_tags :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe [Tag])
createOTAUpdate_tags = Lens.lens (\CreateOTAUpdate' {tags} -> tags) (\s@CreateOTAUpdate' {} a -> s {tags = a} :: CreateOTAUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the update will continue to run (CONTINUOUS), or will
-- be complete after all the things specified as targets have completed the
-- update (SNAPSHOT). If continuous, the update may also be run on a thing
-- when a change is detected in a target. For example, an update will run
-- on a thing when the thing is added to a target group, even after the
-- update was completed by all things originally in the group. Valid
-- values: CONTINUOUS | SNAPSHOT.
createOTAUpdate_targetSelection :: Lens.Lens' CreateOTAUpdate (Prelude.Maybe TargetSelection)
createOTAUpdate_targetSelection = Lens.lens (\CreateOTAUpdate' {targetSelection} -> targetSelection) (\s@CreateOTAUpdate' {} a -> s {targetSelection = a} :: CreateOTAUpdate)

-- | The ID of the OTA update to be created.
createOTAUpdate_otaUpdateId :: Lens.Lens' CreateOTAUpdate Prelude.Text
createOTAUpdate_otaUpdateId = Lens.lens (\CreateOTAUpdate' {otaUpdateId} -> otaUpdateId) (\s@CreateOTAUpdate' {} a -> s {otaUpdateId = a} :: CreateOTAUpdate)

-- | The devices targeted to receive OTA updates.
createOTAUpdate_targets :: Lens.Lens' CreateOTAUpdate (Prelude.NonEmpty Prelude.Text)
createOTAUpdate_targets = Lens.lens (\CreateOTAUpdate' {targets} -> targets) (\s@CreateOTAUpdate' {} a -> s {targets = a} :: CreateOTAUpdate) Prelude.. Lens.coerced

-- | The files to be streamed by the OTA update.
createOTAUpdate_files :: Lens.Lens' CreateOTAUpdate (Prelude.NonEmpty OTAUpdateFile)
createOTAUpdate_files = Lens.lens (\CreateOTAUpdate' {files} -> files) (\s@CreateOTAUpdate' {} a -> s {files = a} :: CreateOTAUpdate) Prelude.. Lens.coerced

-- | The IAM role that grants Amazon Web Services IoT Core access to the
-- Amazon S3, IoT jobs and Amazon Web Services Code Signing resources to
-- create an OTA update job.
createOTAUpdate_roleArn :: Lens.Lens' CreateOTAUpdate Prelude.Text
createOTAUpdate_roleArn = Lens.lens (\CreateOTAUpdate' {roleArn} -> roleArn) (\s@CreateOTAUpdate' {} a -> s {roleArn = a} :: CreateOTAUpdate)

instance Core.AWSRequest CreateOTAUpdate where
  type
    AWSResponse CreateOTAUpdate =
      CreateOTAUpdateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOTAUpdateResponse'
            Prelude.<$> (x Data..?> "awsIotJobArn")
            Prelude.<*> (x Data..?> "awsIotJobId")
            Prelude.<*> (x Data..?> "otaUpdateArn")
            Prelude.<*> (x Data..?> "otaUpdateId")
            Prelude.<*> (x Data..?> "otaUpdateStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOTAUpdate where
  hashWithSalt _salt CreateOTAUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` additionalParameters
      `Prelude.hashWithSalt` awsJobAbortConfig
      `Prelude.hashWithSalt` awsJobExecutionsRolloutConfig
      `Prelude.hashWithSalt` awsJobPresignedUrlConfig
      `Prelude.hashWithSalt` awsJobTimeoutConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetSelection
      `Prelude.hashWithSalt` otaUpdateId
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` files
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateOTAUpdate where
  rnf CreateOTAUpdate' {..} =
    Prelude.rnf additionalParameters `Prelude.seq`
      Prelude.rnf awsJobAbortConfig `Prelude.seq`
        Prelude.rnf awsJobExecutionsRolloutConfig `Prelude.seq`
          Prelude.rnf awsJobPresignedUrlConfig `Prelude.seq`
            Prelude.rnf awsJobTimeoutConfig `Prelude.seq`
              Prelude.rnf description `Prelude.seq`
                Prelude.rnf protocols `Prelude.seq`
                  Prelude.rnf tags `Prelude.seq`
                    Prelude.rnf targetSelection `Prelude.seq`
                      Prelude.rnf otaUpdateId `Prelude.seq`
                        Prelude.rnf targets `Prelude.seq`
                          Prelude.rnf files `Prelude.seq`
                            Prelude.rnf roleArn

instance Data.ToHeaders CreateOTAUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateOTAUpdate where
  toJSON CreateOTAUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalParameters" Data..=)
              Prelude.<$> additionalParameters,
            ("awsJobAbortConfig" Data..=)
              Prelude.<$> awsJobAbortConfig,
            ("awsJobExecutionsRolloutConfig" Data..=)
              Prelude.<$> awsJobExecutionsRolloutConfig,
            ("awsJobPresignedUrlConfig" Data..=)
              Prelude.<$> awsJobPresignedUrlConfig,
            ("awsJobTimeoutConfig" Data..=)
              Prelude.<$> awsJobTimeoutConfig,
            ("description" Data..=) Prelude.<$> description,
            ("protocols" Data..=) Prelude.<$> protocols,
            ("tags" Data..=) Prelude.<$> tags,
            ("targetSelection" Data..=)
              Prelude.<$> targetSelection,
            Prelude.Just ("targets" Data..= targets),
            Prelude.Just ("files" Data..= files),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateOTAUpdate where
  toPath CreateOTAUpdate' {..} =
    Prelude.mconcat
      ["/otaUpdates/", Data.toBS otaUpdateId]

instance Data.ToQuery CreateOTAUpdate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOTAUpdateResponse' smart constructor.
data CreateOTAUpdateResponse = CreateOTAUpdateResponse'
  { -- | The IoT job ARN associated with the OTA update.
    awsIotJobArn :: Prelude.Maybe Prelude.Text,
    -- | The IoT job ID associated with the OTA update.
    awsIotJobId :: Prelude.Maybe Prelude.Text,
    -- | The OTA update ARN.
    otaUpdateArn :: Prelude.Maybe Prelude.Text,
    -- | The OTA update ID.
    otaUpdateId :: Prelude.Maybe Prelude.Text,
    -- | The OTA update status.
    otaUpdateStatus :: Prelude.Maybe OTAUpdateStatus,
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
-- 'awsIotJobArn', 'createOTAUpdateResponse_awsIotJobArn' - The IoT job ARN associated with the OTA update.
--
-- 'awsIotJobId', 'createOTAUpdateResponse_awsIotJobId' - The IoT job ID associated with the OTA update.
--
-- 'otaUpdateArn', 'createOTAUpdateResponse_otaUpdateArn' - The OTA update ARN.
--
-- 'otaUpdateId', 'createOTAUpdateResponse_otaUpdateId' - The OTA update ID.
--
-- 'otaUpdateStatus', 'createOTAUpdateResponse_otaUpdateStatus' - The OTA update status.
--
-- 'httpStatus', 'createOTAUpdateResponse_httpStatus' - The response's http status code.
newCreateOTAUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOTAUpdateResponse
newCreateOTAUpdateResponse pHttpStatus_ =
  CreateOTAUpdateResponse'
    { awsIotJobArn =
        Prelude.Nothing,
      awsIotJobId = Prelude.Nothing,
      otaUpdateArn = Prelude.Nothing,
      otaUpdateId = Prelude.Nothing,
      otaUpdateStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IoT job ARN associated with the OTA update.
createOTAUpdateResponse_awsIotJobArn :: Lens.Lens' CreateOTAUpdateResponse (Prelude.Maybe Prelude.Text)
createOTAUpdateResponse_awsIotJobArn = Lens.lens (\CreateOTAUpdateResponse' {awsIotJobArn} -> awsIotJobArn) (\s@CreateOTAUpdateResponse' {} a -> s {awsIotJobArn = a} :: CreateOTAUpdateResponse)

-- | The IoT job ID associated with the OTA update.
createOTAUpdateResponse_awsIotJobId :: Lens.Lens' CreateOTAUpdateResponse (Prelude.Maybe Prelude.Text)
createOTAUpdateResponse_awsIotJobId = Lens.lens (\CreateOTAUpdateResponse' {awsIotJobId} -> awsIotJobId) (\s@CreateOTAUpdateResponse' {} a -> s {awsIotJobId = a} :: CreateOTAUpdateResponse)

-- | The OTA update ARN.
createOTAUpdateResponse_otaUpdateArn :: Lens.Lens' CreateOTAUpdateResponse (Prelude.Maybe Prelude.Text)
createOTAUpdateResponse_otaUpdateArn = Lens.lens (\CreateOTAUpdateResponse' {otaUpdateArn} -> otaUpdateArn) (\s@CreateOTAUpdateResponse' {} a -> s {otaUpdateArn = a} :: CreateOTAUpdateResponse)

-- | The OTA update ID.
createOTAUpdateResponse_otaUpdateId :: Lens.Lens' CreateOTAUpdateResponse (Prelude.Maybe Prelude.Text)
createOTAUpdateResponse_otaUpdateId = Lens.lens (\CreateOTAUpdateResponse' {otaUpdateId} -> otaUpdateId) (\s@CreateOTAUpdateResponse' {} a -> s {otaUpdateId = a} :: CreateOTAUpdateResponse)

-- | The OTA update status.
createOTAUpdateResponse_otaUpdateStatus :: Lens.Lens' CreateOTAUpdateResponse (Prelude.Maybe OTAUpdateStatus)
createOTAUpdateResponse_otaUpdateStatus = Lens.lens (\CreateOTAUpdateResponse' {otaUpdateStatus} -> otaUpdateStatus) (\s@CreateOTAUpdateResponse' {} a -> s {otaUpdateStatus = a} :: CreateOTAUpdateResponse)

-- | The response's http status code.
createOTAUpdateResponse_httpStatus :: Lens.Lens' CreateOTAUpdateResponse Prelude.Int
createOTAUpdateResponse_httpStatus = Lens.lens (\CreateOTAUpdateResponse' {httpStatus} -> httpStatus) (\s@CreateOTAUpdateResponse' {} a -> s {httpStatus = a} :: CreateOTAUpdateResponse)

instance Prelude.NFData CreateOTAUpdateResponse where
  rnf CreateOTAUpdateResponse' {..} =
    Prelude.rnf awsIotJobArn `Prelude.seq`
      Prelude.rnf awsIotJobId `Prelude.seq`
        Prelude.rnf otaUpdateArn `Prelude.seq`
          Prelude.rnf otaUpdateId `Prelude.seq`
            Prelude.rnf otaUpdateStatus `Prelude.seq`
              Prelude.rnf httpStatus

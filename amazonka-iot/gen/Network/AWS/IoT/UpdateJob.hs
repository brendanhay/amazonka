{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.UpdateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates supported fields of the specified job.
module Network.AWS.IoT.UpdateJob
  ( -- * Creating a Request
    UpdateJob (..),
    newUpdateJob,

    -- * Request Lenses
    updateJob_jobExecutionsRolloutConfig,
    updateJob_timeoutConfig,
    updateJob_namespaceId,
    updateJob_presignedUrlConfig,
    updateJob_description,
    updateJob_abortConfig,
    updateJob_jobId,

    -- * Destructuring the Response
    UpdateJobResponse (..),
    newUpdateJobResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { -- | Allows you to create a staged rollout of the job.
    jobExecutionsRolloutConfig :: Prelude.Maybe JobExecutionsRolloutConfig,
    -- | Specifies the amount of time each device has to finish its execution of
    -- the job. The timer is started when the job execution status is set to
    -- @IN_PROGRESS@. If the job execution status is not set to another
    -- terminal state before the time expires, it will be automatically set to
    -- @TIMED_OUT@.
    timeoutConfig :: Prelude.Maybe TimeoutConfig,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs
    -- notifications to MQTT topics that contain the value in the following
    -- format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for pre-signed S3 URLs.
    presignedUrlConfig :: Prelude.Maybe PresignedUrlConfig,
    -- | A short text description of the job.
    description :: Prelude.Maybe Prelude.Text,
    -- | Allows you to create criteria to abort a job.
    abortConfig :: Prelude.Maybe AbortConfig,
    -- | The ID of the job to be updated.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobExecutionsRolloutConfig', 'updateJob_jobExecutionsRolloutConfig' - Allows you to create a staged rollout of the job.
--
-- 'timeoutConfig', 'updateJob_timeoutConfig' - Specifies the amount of time each device has to finish its execution of
-- the job. The timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the time expires, it will be automatically set to
-- @TIMED_OUT@.
--
-- 'namespaceId', 'updateJob_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'presignedUrlConfig', 'updateJob_presignedUrlConfig' - Configuration information for pre-signed S3 URLs.
--
-- 'description', 'updateJob_description' - A short text description of the job.
--
-- 'abortConfig', 'updateJob_abortConfig' - Allows you to create criteria to abort a job.
--
-- 'jobId', 'updateJob_jobId' - The ID of the job to be updated.
newUpdateJob ::
  -- | 'jobId'
  Prelude.Text ->
  UpdateJob
newUpdateJob pJobId_ =
  UpdateJob'
    { jobExecutionsRolloutConfig =
        Prelude.Nothing,
      timeoutConfig = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      presignedUrlConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      abortConfig = Prelude.Nothing,
      jobId = pJobId_
    }

-- | Allows you to create a staged rollout of the job.
updateJob_jobExecutionsRolloutConfig :: Lens.Lens' UpdateJob (Prelude.Maybe JobExecutionsRolloutConfig)
updateJob_jobExecutionsRolloutConfig = Lens.lens (\UpdateJob' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@UpdateJob' {} a -> s {jobExecutionsRolloutConfig = a} :: UpdateJob)

-- | Specifies the amount of time each device has to finish its execution of
-- the job. The timer is started when the job execution status is set to
-- @IN_PROGRESS@. If the job execution status is not set to another
-- terminal state before the time expires, it will be automatically set to
-- @TIMED_OUT@.
updateJob_timeoutConfig :: Lens.Lens' UpdateJob (Prelude.Maybe TimeoutConfig)
updateJob_timeoutConfig = Lens.lens (\UpdateJob' {timeoutConfig} -> timeoutConfig) (\s@UpdateJob' {} a -> s {timeoutConfig = a} :: UpdateJob)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
updateJob_namespaceId :: Lens.Lens' UpdateJob (Prelude.Maybe Prelude.Text)
updateJob_namespaceId = Lens.lens (\UpdateJob' {namespaceId} -> namespaceId) (\s@UpdateJob' {} a -> s {namespaceId = a} :: UpdateJob)

-- | Configuration information for pre-signed S3 URLs.
updateJob_presignedUrlConfig :: Lens.Lens' UpdateJob (Prelude.Maybe PresignedUrlConfig)
updateJob_presignedUrlConfig = Lens.lens (\UpdateJob' {presignedUrlConfig} -> presignedUrlConfig) (\s@UpdateJob' {} a -> s {presignedUrlConfig = a} :: UpdateJob)

-- | A short text description of the job.
updateJob_description :: Lens.Lens' UpdateJob (Prelude.Maybe Prelude.Text)
updateJob_description = Lens.lens (\UpdateJob' {description} -> description) (\s@UpdateJob' {} a -> s {description = a} :: UpdateJob)

-- | Allows you to create criteria to abort a job.
updateJob_abortConfig :: Lens.Lens' UpdateJob (Prelude.Maybe AbortConfig)
updateJob_abortConfig = Lens.lens (\UpdateJob' {abortConfig} -> abortConfig) (\s@UpdateJob' {} a -> s {abortConfig = a} :: UpdateJob)

-- | The ID of the job to be updated.
updateJob_jobId :: Lens.Lens' UpdateJob Prelude.Text
updateJob_jobId = Lens.lens (\UpdateJob' {jobId} -> jobId) (\s@UpdateJob' {} a -> s {jobId = a} :: UpdateJob)

instance Prelude.AWSRequest UpdateJob where
  type Rs UpdateJob = UpdateJobResponse
  request = Request.patchJSON defaultService
  response = Response.receiveNull UpdateJobResponse'

instance Prelude.Hashable UpdateJob

instance Prelude.NFData UpdateJob

instance Prelude.ToHeaders UpdateJob where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateJob where
  toJSON UpdateJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("jobExecutionsRolloutConfig" Prelude..=)
              Prelude.<$> jobExecutionsRolloutConfig,
            ("timeoutConfig" Prelude..=)
              Prelude.<$> timeoutConfig,
            ("presignedUrlConfig" Prelude..=)
              Prelude.<$> presignedUrlConfig,
            ("description" Prelude..=) Prelude.<$> description,
            ("abortConfig" Prelude..=) Prelude.<$> abortConfig
          ]
      )

instance Prelude.ToPath UpdateJob where
  toPath UpdateJob' {..} =
    Prelude.mconcat ["/jobs/", Prelude.toBS jobId]

instance Prelude.ToQuery UpdateJob where
  toQuery UpdateJob' {..} =
    Prelude.mconcat
      ["namespaceId" Prelude.=: namespaceId]

-- | /See:/ 'newUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateJobResponse ::
  UpdateJobResponse
newUpdateJobResponse = UpdateJobResponse'

instance Prelude.NFData UpdateJobResponse

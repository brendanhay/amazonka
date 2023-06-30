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
-- Module      : Amazonka.Panorama.CreateJobForDevices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a job to run on one or more devices. A job can update a
-- device\'s software or reboot it.
module Amazonka.Panorama.CreateJobForDevices
  ( -- * Creating a Request
    CreateJobForDevices (..),
    newCreateJobForDevices,

    -- * Request Lenses
    createJobForDevices_deviceJobConfig,
    createJobForDevices_deviceIds,
    createJobForDevices_jobType,

    -- * Destructuring the Response
    CreateJobForDevicesResponse (..),
    newCreateJobForDevicesResponse,

    -- * Response Lenses
    createJobForDevicesResponse_httpStatus,
    createJobForDevicesResponse_jobs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateJobForDevices' smart constructor.
data CreateJobForDevices = CreateJobForDevices'
  { -- | Configuration settings for a software update job.
    deviceJobConfig :: Prelude.Maybe DeviceJobConfig,
    -- | IDs of target devices.
    deviceIds :: Prelude.NonEmpty Prelude.Text,
    -- | The type of job to run.
    jobType :: JobType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobForDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceJobConfig', 'createJobForDevices_deviceJobConfig' - Configuration settings for a software update job.
--
-- 'deviceIds', 'createJobForDevices_deviceIds' - IDs of target devices.
--
-- 'jobType', 'createJobForDevices_jobType' - The type of job to run.
newCreateJobForDevices ::
  -- | 'deviceIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'jobType'
  JobType ->
  CreateJobForDevices
newCreateJobForDevices pDeviceIds_ pJobType_ =
  CreateJobForDevices'
    { deviceJobConfig =
        Prelude.Nothing,
      deviceIds = Lens.coerced Lens.# pDeviceIds_,
      jobType = pJobType_
    }

-- | Configuration settings for a software update job.
createJobForDevices_deviceJobConfig :: Lens.Lens' CreateJobForDevices (Prelude.Maybe DeviceJobConfig)
createJobForDevices_deviceJobConfig = Lens.lens (\CreateJobForDevices' {deviceJobConfig} -> deviceJobConfig) (\s@CreateJobForDevices' {} a -> s {deviceJobConfig = a} :: CreateJobForDevices)

-- | IDs of target devices.
createJobForDevices_deviceIds :: Lens.Lens' CreateJobForDevices (Prelude.NonEmpty Prelude.Text)
createJobForDevices_deviceIds = Lens.lens (\CreateJobForDevices' {deviceIds} -> deviceIds) (\s@CreateJobForDevices' {} a -> s {deviceIds = a} :: CreateJobForDevices) Prelude.. Lens.coerced

-- | The type of job to run.
createJobForDevices_jobType :: Lens.Lens' CreateJobForDevices JobType
createJobForDevices_jobType = Lens.lens (\CreateJobForDevices' {jobType} -> jobType) (\s@CreateJobForDevices' {} a -> s {jobType = a} :: CreateJobForDevices)

instance Core.AWSRequest CreateJobForDevices where
  type
    AWSResponse CreateJobForDevices =
      CreateJobForDevicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobForDevicesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Jobs" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable CreateJobForDevices where
  hashWithSalt _salt CreateJobForDevices' {..} =
    _salt
      `Prelude.hashWithSalt` deviceJobConfig
      `Prelude.hashWithSalt` deviceIds
      `Prelude.hashWithSalt` jobType

instance Prelude.NFData CreateJobForDevices where
  rnf CreateJobForDevices' {..} =
    Prelude.rnf deviceJobConfig
      `Prelude.seq` Prelude.rnf deviceIds
      `Prelude.seq` Prelude.rnf jobType

instance Data.ToHeaders CreateJobForDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateJobForDevices where
  toJSON CreateJobForDevices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceJobConfig" Data..=)
              Prelude.<$> deviceJobConfig,
            Prelude.Just ("DeviceIds" Data..= deviceIds),
            Prelude.Just ("JobType" Data..= jobType)
          ]
      )

instance Data.ToPath CreateJobForDevices where
  toPath = Prelude.const "/jobs"

instance Data.ToQuery CreateJobForDevices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateJobForDevicesResponse' smart constructor.
data CreateJobForDevicesResponse = CreateJobForDevicesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of jobs.
    jobs :: [Job]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobForDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createJobForDevicesResponse_httpStatus' - The response's http status code.
--
-- 'jobs', 'createJobForDevicesResponse_jobs' - A list of jobs.
newCreateJobForDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateJobForDevicesResponse
newCreateJobForDevicesResponse pHttpStatus_ =
  CreateJobForDevicesResponse'
    { httpStatus =
        pHttpStatus_,
      jobs = Prelude.mempty
    }

-- | The response's http status code.
createJobForDevicesResponse_httpStatus :: Lens.Lens' CreateJobForDevicesResponse Prelude.Int
createJobForDevicesResponse_httpStatus = Lens.lens (\CreateJobForDevicesResponse' {httpStatus} -> httpStatus) (\s@CreateJobForDevicesResponse' {} a -> s {httpStatus = a} :: CreateJobForDevicesResponse)

-- | A list of jobs.
createJobForDevicesResponse_jobs :: Lens.Lens' CreateJobForDevicesResponse [Job]
createJobForDevicesResponse_jobs = Lens.lens (\CreateJobForDevicesResponse' {jobs} -> jobs) (\s@CreateJobForDevicesResponse' {} a -> s {jobs = a} :: CreateJobForDevicesResponse) Prelude.. Lens.coerced

instance Prelude.NFData CreateJobForDevicesResponse where
  rnf CreateJobForDevicesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf jobs

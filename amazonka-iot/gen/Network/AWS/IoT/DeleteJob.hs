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
-- Module      : Network.AWS.IoT.DeleteJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a job and its related job executions.
--
-- Deleting a job may take time, depending on the number of job executions
-- created for the job and various other factors. While the job is being
-- deleted, the status of the job will be shown as
-- \"DELETION_IN_PROGRESS\". Attempting to delete or cancel a job whose
-- status is already \"DELETION_IN_PROGRESS\" will result in an error.
--
-- Only 10 jobs may have status \"DELETION_IN_PROGRESS\" at the same time,
-- or a LimitExceededException will occur.
module Network.AWS.IoT.DeleteJob
  ( -- * Creating a Request
    DeleteJob (..),
    newDeleteJob,

    -- * Request Lenses
    deleteJob_namespaceId,
    deleteJob_force,
    deleteJob_jobId,

    -- * Destructuring the Response
    DeleteJobResponse (..),
    newDeleteJobResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteJob' smart constructor.
data DeleteJob = DeleteJob'
  { -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs
    -- notifications to MQTT topics that contain the value in the following
    -- format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) When true, you can delete a job which is \"IN_PROGRESS\".
    -- Otherwise, you can only delete a job which is in a terminal state
    -- (\"COMPLETED\" or \"CANCELED\") or an exception will occur. The default
    -- is false.
    --
    -- Deleting a job which is \"IN_PROGRESS\", will cause a device which is
    -- executing the job to be unable to access job information or update the
    -- job execution status. Use caution and ensure that each device executing
    -- a job which is deleted is able to recover to a valid state.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the job to be deleted.
    --
    -- After a job deletion is completed, you may reuse this jobId when you
    -- create a new job. However, this is not recommended, and you must ensure
    -- that your devices are not using the jobId to refer to the deleted job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceId', 'deleteJob_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'force', 'deleteJob_force' - (Optional) When true, you can delete a job which is \"IN_PROGRESS\".
-- Otherwise, you can only delete a job which is in a terminal state
-- (\"COMPLETED\" or \"CANCELED\") or an exception will occur. The default
-- is false.
--
-- Deleting a job which is \"IN_PROGRESS\", will cause a device which is
-- executing the job to be unable to access job information or update the
-- job execution status. Use caution and ensure that each device executing
-- a job which is deleted is able to recover to a valid state.
--
-- 'jobId', 'deleteJob_jobId' - The ID of the job to be deleted.
--
-- After a job deletion is completed, you may reuse this jobId when you
-- create a new job. However, this is not recommended, and you must ensure
-- that your devices are not using the jobId to refer to the deleted job.
newDeleteJob ::
  -- | 'jobId'
  Prelude.Text ->
  DeleteJob
newDeleteJob pJobId_ =
  DeleteJob'
    { namespaceId = Prelude.Nothing,
      force = Prelude.Nothing,
      jobId = pJobId_
    }

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
deleteJob_namespaceId :: Lens.Lens' DeleteJob (Prelude.Maybe Prelude.Text)
deleteJob_namespaceId = Lens.lens (\DeleteJob' {namespaceId} -> namespaceId) (\s@DeleteJob' {} a -> s {namespaceId = a} :: DeleteJob)

-- | (Optional) When true, you can delete a job which is \"IN_PROGRESS\".
-- Otherwise, you can only delete a job which is in a terminal state
-- (\"COMPLETED\" or \"CANCELED\") or an exception will occur. The default
-- is false.
--
-- Deleting a job which is \"IN_PROGRESS\", will cause a device which is
-- executing the job to be unable to access job information or update the
-- job execution status. Use caution and ensure that each device executing
-- a job which is deleted is able to recover to a valid state.
deleteJob_force :: Lens.Lens' DeleteJob (Prelude.Maybe Prelude.Bool)
deleteJob_force = Lens.lens (\DeleteJob' {force} -> force) (\s@DeleteJob' {} a -> s {force = a} :: DeleteJob)

-- | The ID of the job to be deleted.
--
-- After a job deletion is completed, you may reuse this jobId when you
-- create a new job. However, this is not recommended, and you must ensure
-- that your devices are not using the jobId to refer to the deleted job.
deleteJob_jobId :: Lens.Lens' DeleteJob Prelude.Text
deleteJob_jobId = Lens.lens (\DeleteJob' {jobId} -> jobId) (\s@DeleteJob' {} a -> s {jobId = a} :: DeleteJob)

instance Prelude.AWSRequest DeleteJob where
  type Rs DeleteJob = DeleteJobResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteJobResponse'

instance Prelude.Hashable DeleteJob

instance Prelude.NFData DeleteJob

instance Prelude.ToHeaders DeleteJob where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteJob where
  toPath DeleteJob' {..} =
    Prelude.mconcat ["/jobs/", Prelude.toBS jobId]

instance Prelude.ToQuery DeleteJob where
  toQuery DeleteJob' {..} =
    Prelude.mconcat
      [ "namespaceId" Prelude.=: namespaceId,
        "force" Prelude.=: force
      ]

-- | /See:/ 'newDeleteJobResponse' smart constructor.
data DeleteJobResponse = DeleteJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteJobResponse ::
  DeleteJobResponse
newDeleteJobResponse = DeleteJobResponse'

instance Prelude.NFData DeleteJobResponse

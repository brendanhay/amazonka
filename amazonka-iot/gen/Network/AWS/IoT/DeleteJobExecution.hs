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
-- Module      : Network.AWS.IoT.DeleteJobExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a job execution.
module Network.AWS.IoT.DeleteJobExecution
  ( -- * Creating a Request
    DeleteJobExecution (..),
    newDeleteJobExecution,

    -- * Request Lenses
    deleteJobExecution_namespaceId,
    deleteJobExecution_force,
    deleteJobExecution_jobId,
    deleteJobExecution_thingName,
    deleteJobExecution_executionNumber,

    -- * Destructuring the Response
    DeleteJobExecutionResponse (..),
    newDeleteJobExecutionResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteJobExecution' smart constructor.
data DeleteJobExecution = DeleteJobExecution'
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
    -- | (Optional) When true, you can delete a job execution which is
    -- \"IN_PROGRESS\". Otherwise, you can only delete a job execution which is
    -- in a terminal state (\"SUCCEEDED\", \"FAILED\", \"REJECTED\",
    -- \"REMOVED\" or \"CANCELED\") or an exception will occur. The default is
    -- false.
    --
    -- Deleting a job execution which is \"IN_PROGRESS\", will cause the device
    -- to be unable to access job information or update the job execution
    -- status. Use caution and ensure that the device is able to recover to a
    -- valid state.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the job whose execution on a particular device will be
    -- deleted.
    jobId :: Prelude.Text,
    -- | The name of the thing whose job execution will be deleted.
    thingName :: Prelude.Text,
    -- | The ID of the job execution to be deleted. The @executionNumber@ refers
    -- to the execution of a particular job on a particular device.
    --
    -- Note that once a job execution is deleted, the @executionNumber@ may be
    -- reused by IoT, so be sure you get and use the correct value here.
    executionNumber :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteJobExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceId', 'deleteJobExecution_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'force', 'deleteJobExecution_force' - (Optional) When true, you can delete a job execution which is
-- \"IN_PROGRESS\". Otherwise, you can only delete a job execution which is
-- in a terminal state (\"SUCCEEDED\", \"FAILED\", \"REJECTED\",
-- \"REMOVED\" or \"CANCELED\") or an exception will occur. The default is
-- false.
--
-- Deleting a job execution which is \"IN_PROGRESS\", will cause the device
-- to be unable to access job information or update the job execution
-- status. Use caution and ensure that the device is able to recover to a
-- valid state.
--
-- 'jobId', 'deleteJobExecution_jobId' - The ID of the job whose execution on a particular device will be
-- deleted.
--
-- 'thingName', 'deleteJobExecution_thingName' - The name of the thing whose job execution will be deleted.
--
-- 'executionNumber', 'deleteJobExecution_executionNumber' - The ID of the job execution to be deleted. The @executionNumber@ refers
-- to the execution of a particular job on a particular device.
--
-- Note that once a job execution is deleted, the @executionNumber@ may be
-- reused by IoT, so be sure you get and use the correct value here.
newDeleteJobExecution ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'thingName'
  Prelude.Text ->
  -- | 'executionNumber'
  Prelude.Integer ->
  DeleteJobExecution
newDeleteJobExecution
  pJobId_
  pThingName_
  pExecutionNumber_ =
    DeleteJobExecution'
      { namespaceId = Prelude.Nothing,
        force = Prelude.Nothing,
        jobId = pJobId_,
        thingName = pThingName_,
        executionNumber = pExecutionNumber_
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
deleteJobExecution_namespaceId :: Lens.Lens' DeleteJobExecution (Prelude.Maybe Prelude.Text)
deleteJobExecution_namespaceId = Lens.lens (\DeleteJobExecution' {namespaceId} -> namespaceId) (\s@DeleteJobExecution' {} a -> s {namespaceId = a} :: DeleteJobExecution)

-- | (Optional) When true, you can delete a job execution which is
-- \"IN_PROGRESS\". Otherwise, you can only delete a job execution which is
-- in a terminal state (\"SUCCEEDED\", \"FAILED\", \"REJECTED\",
-- \"REMOVED\" or \"CANCELED\") or an exception will occur. The default is
-- false.
--
-- Deleting a job execution which is \"IN_PROGRESS\", will cause the device
-- to be unable to access job information or update the job execution
-- status. Use caution and ensure that the device is able to recover to a
-- valid state.
deleteJobExecution_force :: Lens.Lens' DeleteJobExecution (Prelude.Maybe Prelude.Bool)
deleteJobExecution_force = Lens.lens (\DeleteJobExecution' {force} -> force) (\s@DeleteJobExecution' {} a -> s {force = a} :: DeleteJobExecution)

-- | The ID of the job whose execution on a particular device will be
-- deleted.
deleteJobExecution_jobId :: Lens.Lens' DeleteJobExecution Prelude.Text
deleteJobExecution_jobId = Lens.lens (\DeleteJobExecution' {jobId} -> jobId) (\s@DeleteJobExecution' {} a -> s {jobId = a} :: DeleteJobExecution)

-- | The name of the thing whose job execution will be deleted.
deleteJobExecution_thingName :: Lens.Lens' DeleteJobExecution Prelude.Text
deleteJobExecution_thingName = Lens.lens (\DeleteJobExecution' {thingName} -> thingName) (\s@DeleteJobExecution' {} a -> s {thingName = a} :: DeleteJobExecution)

-- | The ID of the job execution to be deleted. The @executionNumber@ refers
-- to the execution of a particular job on a particular device.
--
-- Note that once a job execution is deleted, the @executionNumber@ may be
-- reused by IoT, so be sure you get and use the correct value here.
deleteJobExecution_executionNumber :: Lens.Lens' DeleteJobExecution Prelude.Integer
deleteJobExecution_executionNumber = Lens.lens (\DeleteJobExecution' {executionNumber} -> executionNumber) (\s@DeleteJobExecution' {} a -> s {executionNumber = a} :: DeleteJobExecution)

instance Prelude.AWSRequest DeleteJobExecution where
  type
    Rs DeleteJobExecution =
      DeleteJobExecutionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteJobExecutionResponse'

instance Prelude.Hashable DeleteJobExecution

instance Prelude.NFData DeleteJobExecution

instance Prelude.ToHeaders DeleteJobExecution where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteJobExecution where
  toPath DeleteJobExecution' {..} =
    Prelude.mconcat
      [ "/things/",
        Prelude.toBS thingName,
        "/jobs/",
        Prelude.toBS jobId,
        "/executionNumber/",
        Prelude.toBS executionNumber
      ]

instance Prelude.ToQuery DeleteJobExecution where
  toQuery DeleteJobExecution' {..} =
    Prelude.mconcat
      [ "namespaceId" Prelude.=: namespaceId,
        "force" Prelude.=: force
      ]

-- | /See:/ 'newDeleteJobExecutionResponse' smart constructor.
data DeleteJobExecutionResponse = DeleteJobExecutionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteJobExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteJobExecutionResponse ::
  DeleteJobExecutionResponse
newDeleteJobExecutionResponse =
  DeleteJobExecutionResponse'

instance Prelude.NFData DeleteJobExecutionResponse

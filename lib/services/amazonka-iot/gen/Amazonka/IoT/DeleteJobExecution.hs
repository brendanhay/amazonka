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
-- Module      : Amazonka.IoT.DeleteJobExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a job execution.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteJobExecution>
-- action.
module Amazonka.IoT.DeleteJobExecution
  ( -- * Creating a Request
    DeleteJobExecution (..),
    newDeleteJobExecution,

    -- * Request Lenses
    deleteJobExecution_force,
    deleteJobExecution_namespaceId,
    deleteJobExecution_jobId,
    deleteJobExecution_thingName,
    deleteJobExecution_executionNumber,

    -- * Destructuring the Response
    DeleteJobExecutionResponse (..),
    newDeleteJobExecutionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteJobExecution' smart constructor.
data DeleteJobExecution = DeleteJobExecution'
  { -- | (Optional) When true, you can delete a job execution which is
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
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, Amazon Web Services IoT
    -- Core sends jobs notifications to MQTT topics that contain the value in
    -- the following format.
    --
    -- @$aws\/things\/@/@THING_NAME@/@\/jobs\/@/@JOB_ID@/@\/notify-namespace-@/@NAMESPACE_ID@/@\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteJobExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'namespaceId', 'deleteJobExecution_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/@/@THING_NAME@/@\/jobs\/@/@JOB_ID@/@\/notify-namespace-@/@NAMESPACE_ID@/@\/@
--
-- The @namespaceId@ feature is in public preview.
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
      { force = Prelude.Nothing,
        namespaceId = Prelude.Nothing,
        jobId = pJobId_,
        thingName = pThingName_,
        executionNumber = pExecutionNumber_
      }

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

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/@/@THING_NAME@/@\/jobs\/@/@JOB_ID@/@\/notify-namespace-@/@NAMESPACE_ID@/@\/@
--
-- The @namespaceId@ feature is in public preview.
deleteJobExecution_namespaceId :: Lens.Lens' DeleteJobExecution (Prelude.Maybe Prelude.Text)
deleteJobExecution_namespaceId = Lens.lens (\DeleteJobExecution' {namespaceId} -> namespaceId) (\s@DeleteJobExecution' {} a -> s {namespaceId = a} :: DeleteJobExecution)

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

instance Core.AWSRequest DeleteJobExecution where
  type
    AWSResponse DeleteJobExecution =
      DeleteJobExecutionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteJobExecutionResponse'

instance Prelude.Hashable DeleteJobExecution where
  hashWithSalt _salt DeleteJobExecution' {..} =
    _salt
      `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` namespaceId
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` executionNumber

instance Prelude.NFData DeleteJobExecution where
  rnf DeleteJobExecution' {..} =
    Prelude.rnf force
      `Prelude.seq` Prelude.rnf namespaceId
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf executionNumber

instance Data.ToHeaders DeleteJobExecution where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteJobExecution where
  toPath DeleteJobExecution' {..} =
    Prelude.mconcat
      [ "/things/",
        Data.toBS thingName,
        "/jobs/",
        Data.toBS jobId,
        "/executionNumber/",
        Data.toBS executionNumber
      ]

instance Data.ToQuery DeleteJobExecution where
  toQuery DeleteJobExecution' {..} =
    Prelude.mconcat
      [ "force" Data.=: force,
        "namespaceId" Data.=: namespaceId
      ]

-- | /See:/ 'newDeleteJobExecutionResponse' smart constructor.
data DeleteJobExecutionResponse = DeleteJobExecutionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteJobExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteJobExecutionResponse ::
  DeleteJobExecutionResponse
newDeleteJobExecutionResponse =
  DeleteJobExecutionResponse'

instance Prelude.NFData DeleteJobExecutionResponse where
  rnf _ = ()

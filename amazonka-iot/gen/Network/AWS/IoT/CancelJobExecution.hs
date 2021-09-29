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
-- Module      : Network.AWS.IoT.CancelJobExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the execution of a job for a given thing.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CancelJobExecution>
-- action.
module Network.AWS.IoT.CancelJobExecution
  ( -- * Creating a Request
    CancelJobExecution (..),
    newCancelJobExecution,

    -- * Request Lenses
    cancelJobExecution_expectedVersion,
    cancelJobExecution_statusDetails,
    cancelJobExecution_force,
    cancelJobExecution_jobId,
    cancelJobExecution_thingName,

    -- * Destructuring the Response
    CancelJobExecutionResponse (..),
    newCancelJobExecutionResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelJobExecution' smart constructor.
data CancelJobExecution = CancelJobExecution'
  { -- | (Optional) The expected current version of the job execution. Each time
    -- you update the job execution, its version is incremented. If the version
    -- of the job execution stored in Jobs does not match, the update is
    -- rejected with a VersionMismatch error, and an ErrorResponse that
    -- contains the current job execution status data is returned. (This makes
    -- it unnecessary to perform a separate DescribeJobExecution request in
    -- order to obtain the job execution status data.)
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | A collection of name\/value pairs that describe the status of the job
    -- execution. If not specified, the statusDetails are unchanged. You can
    -- specify at most 10 name\/value pairs.
    statusDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | (Optional) If @true@ the job execution will be canceled if it has status
    -- IN_PROGRESS or QUEUED, otherwise the job execution will be canceled only
    -- if it has status QUEUED. If you attempt to cancel a job execution that
    -- is IN_PROGRESS, and you do not set @force@ to @true@, then an
    -- @InvalidStateTransitionException@ will be thrown. The default is
    -- @false@.
    --
    -- Canceling a job execution which is \"IN_PROGRESS\", will cause the
    -- device to be unable to update the job execution status. Use caution and
    -- ensure that the device is able to recover to a valid state.
    force :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the job to be canceled.
    jobId :: Prelude.Text,
    -- | The name of the thing whose execution of the job will be canceled.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJobExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedVersion', 'cancelJobExecution_expectedVersion' - (Optional) The expected current version of the job execution. Each time
-- you update the job execution, its version is incremented. If the version
-- of the job execution stored in Jobs does not match, the update is
-- rejected with a VersionMismatch error, and an ErrorResponse that
-- contains the current job execution status data is returned. (This makes
-- it unnecessary to perform a separate DescribeJobExecution request in
-- order to obtain the job execution status data.)
--
-- 'statusDetails', 'cancelJobExecution_statusDetails' - A collection of name\/value pairs that describe the status of the job
-- execution. If not specified, the statusDetails are unchanged. You can
-- specify at most 10 name\/value pairs.
--
-- 'force', 'cancelJobExecution_force' - (Optional) If @true@ the job execution will be canceled if it has status
-- IN_PROGRESS or QUEUED, otherwise the job execution will be canceled only
-- if it has status QUEUED. If you attempt to cancel a job execution that
-- is IN_PROGRESS, and you do not set @force@ to @true@, then an
-- @InvalidStateTransitionException@ will be thrown. The default is
-- @false@.
--
-- Canceling a job execution which is \"IN_PROGRESS\", will cause the
-- device to be unable to update the job execution status. Use caution and
-- ensure that the device is able to recover to a valid state.
--
-- 'jobId', 'cancelJobExecution_jobId' - The ID of the job to be canceled.
--
-- 'thingName', 'cancelJobExecution_thingName' - The name of the thing whose execution of the job will be canceled.
newCancelJobExecution ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'thingName'
  Prelude.Text ->
  CancelJobExecution
newCancelJobExecution pJobId_ pThingName_ =
  CancelJobExecution'
    { expectedVersion =
        Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      force = Prelude.Nothing,
      jobId = pJobId_,
      thingName = pThingName_
    }

-- | (Optional) The expected current version of the job execution. Each time
-- you update the job execution, its version is incremented. If the version
-- of the job execution stored in Jobs does not match, the update is
-- rejected with a VersionMismatch error, and an ErrorResponse that
-- contains the current job execution status data is returned. (This makes
-- it unnecessary to perform a separate DescribeJobExecution request in
-- order to obtain the job execution status data.)
cancelJobExecution_expectedVersion :: Lens.Lens' CancelJobExecution (Prelude.Maybe Prelude.Integer)
cancelJobExecution_expectedVersion = Lens.lens (\CancelJobExecution' {expectedVersion} -> expectedVersion) (\s@CancelJobExecution' {} a -> s {expectedVersion = a} :: CancelJobExecution)

-- | A collection of name\/value pairs that describe the status of the job
-- execution. If not specified, the statusDetails are unchanged. You can
-- specify at most 10 name\/value pairs.
cancelJobExecution_statusDetails :: Lens.Lens' CancelJobExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
cancelJobExecution_statusDetails = Lens.lens (\CancelJobExecution' {statusDetails} -> statusDetails) (\s@CancelJobExecution' {} a -> s {statusDetails = a} :: CancelJobExecution) Prelude.. Lens.mapping Lens._Coerce

-- | (Optional) If @true@ the job execution will be canceled if it has status
-- IN_PROGRESS or QUEUED, otherwise the job execution will be canceled only
-- if it has status QUEUED. If you attempt to cancel a job execution that
-- is IN_PROGRESS, and you do not set @force@ to @true@, then an
-- @InvalidStateTransitionException@ will be thrown. The default is
-- @false@.
--
-- Canceling a job execution which is \"IN_PROGRESS\", will cause the
-- device to be unable to update the job execution status. Use caution and
-- ensure that the device is able to recover to a valid state.
cancelJobExecution_force :: Lens.Lens' CancelJobExecution (Prelude.Maybe Prelude.Bool)
cancelJobExecution_force = Lens.lens (\CancelJobExecution' {force} -> force) (\s@CancelJobExecution' {} a -> s {force = a} :: CancelJobExecution)

-- | The ID of the job to be canceled.
cancelJobExecution_jobId :: Lens.Lens' CancelJobExecution Prelude.Text
cancelJobExecution_jobId = Lens.lens (\CancelJobExecution' {jobId} -> jobId) (\s@CancelJobExecution' {} a -> s {jobId = a} :: CancelJobExecution)

-- | The name of the thing whose execution of the job will be canceled.
cancelJobExecution_thingName :: Lens.Lens' CancelJobExecution Prelude.Text
cancelJobExecution_thingName = Lens.lens (\CancelJobExecution' {thingName} -> thingName) (\s@CancelJobExecution' {} a -> s {thingName = a} :: CancelJobExecution)

instance Core.AWSRequest CancelJobExecution where
  type
    AWSResponse CancelJobExecution =
      CancelJobExecutionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull CancelJobExecutionResponse'

instance Prelude.Hashable CancelJobExecution

instance Prelude.NFData CancelJobExecution

instance Core.ToHeaders CancelJobExecution where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CancelJobExecution where
  toJSON CancelJobExecution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("expectedVersion" Core..=)
              Prelude.<$> expectedVersion,
            ("statusDetails" Core..=) Prelude.<$> statusDetails
          ]
      )

instance Core.ToPath CancelJobExecution where
  toPath CancelJobExecution' {..} =
    Prelude.mconcat
      [ "/things/",
        Core.toBS thingName,
        "/jobs/",
        Core.toBS jobId,
        "/cancel"
      ]

instance Core.ToQuery CancelJobExecution where
  toQuery CancelJobExecution' {..} =
    Prelude.mconcat ["force" Core.=: force]

-- | /See:/ 'newCancelJobExecutionResponse' smart constructor.
data CancelJobExecutionResponse = CancelJobExecutionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJobExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelJobExecutionResponse ::
  CancelJobExecutionResponse
newCancelJobExecutionResponse =
  CancelJobExecutionResponse'

instance Prelude.NFData CancelJobExecutionResponse

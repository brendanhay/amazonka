{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CancelJobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the execution of a job for a given thing.
module Network.AWS.IoT.CancelJobExecution
  ( -- * Creating a request
    CancelJobExecution (..),
    mkCancelJobExecution,

    -- ** Request lenses
    cjeJobId,
    cjeForce,
    cjeStatusDetails,
    cjeExpectedVersion,
    cjeThingName,

    -- * Destructuring the response
    CancelJobExecutionResponse (..),
    mkCancelJobExecutionResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelJobExecution' smart constructor.
data CancelJobExecution = CancelJobExecution'
  { -- | The ID of the job to be canceled.
    jobId :: Lude.Text,
    -- | (Optional) If @true@ the job execution will be canceled if it has status IN_PROGRESS or QUEUED, otherwise the job execution will be canceled only if it has status QUEUED. If you attempt to cancel a job execution that is IN_PROGRESS, and you do not set @force@ to @true@ , then an @InvalidStateTransitionException@ will be thrown. The default is @false@ .
    --
    -- Canceling a job execution which is "IN_PROGRESS", will cause the device to be unable to update the job execution status. Use caution and ensure that the device is able to recover to a valid state.
    force :: Lude.Maybe Lude.Bool,
    -- | A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged. You can specify at most 10 name/value pairs.
    statusDetails :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | (Optional) The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
    expectedVersion :: Lude.Maybe Lude.Integer,
    -- | The name of the thing whose execution of the job will be canceled.
    thingName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelJobExecution' with the minimum fields required to make a request.
--
-- * 'jobId' - The ID of the job to be canceled.
-- * 'force' - (Optional) If @true@ the job execution will be canceled if it has status IN_PROGRESS or QUEUED, otherwise the job execution will be canceled only if it has status QUEUED. If you attempt to cancel a job execution that is IN_PROGRESS, and you do not set @force@ to @true@ , then an @InvalidStateTransitionException@ will be thrown. The default is @false@ .
--
-- Canceling a job execution which is "IN_PROGRESS", will cause the device to be unable to update the job execution status. Use caution and ensure that the device is able to recover to a valid state.
-- * 'statusDetails' - A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged. You can specify at most 10 name/value pairs.
-- * 'expectedVersion' - (Optional) The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
-- * 'thingName' - The name of the thing whose execution of the job will be canceled.
mkCancelJobExecution ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'thingName'
  Lude.Text ->
  CancelJobExecution
mkCancelJobExecution pJobId_ pThingName_ =
  CancelJobExecution'
    { jobId = pJobId_,
      force = Lude.Nothing,
      statusDetails = Lude.Nothing,
      expectedVersion = Lude.Nothing,
      thingName = pThingName_
    }

-- | The ID of the job to be canceled.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjeJobId :: Lens.Lens' CancelJobExecution Lude.Text
cjeJobId = Lens.lens (jobId :: CancelJobExecution -> Lude.Text) (\s a -> s {jobId = a} :: CancelJobExecution)
{-# DEPRECATED cjeJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | (Optional) If @true@ the job execution will be canceled if it has status IN_PROGRESS or QUEUED, otherwise the job execution will be canceled only if it has status QUEUED. If you attempt to cancel a job execution that is IN_PROGRESS, and you do not set @force@ to @true@ , then an @InvalidStateTransitionException@ will be thrown. The default is @false@ .
--
-- Canceling a job execution which is "IN_PROGRESS", will cause the device to be unable to update the job execution status. Use caution and ensure that the device is able to recover to a valid state.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjeForce :: Lens.Lens' CancelJobExecution (Lude.Maybe Lude.Bool)
cjeForce = Lens.lens (force :: CancelJobExecution -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: CancelJobExecution)
{-# DEPRECATED cjeForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | A collection of name/value pairs that describe the status of the job execution. If not specified, the statusDetails are unchanged. You can specify at most 10 name/value pairs.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjeStatusDetails :: Lens.Lens' CancelJobExecution (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cjeStatusDetails = Lens.lens (statusDetails :: CancelJobExecution -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {statusDetails = a} :: CancelJobExecution)
{-# DEPRECATED cjeStatusDetails "Use generic-lens or generic-optics with 'statusDetails' instead." #-}

-- | (Optional) The expected current version of the job execution. Each time you update the job execution, its version is incremented. If the version of the job execution stored in Jobs does not match, the update is rejected with a VersionMismatch error, and an ErrorResponse that contains the current job execution status data is returned. (This makes it unnecessary to perform a separate DescribeJobExecution request in order to obtain the job execution status data.)
--
-- /Note:/ Consider using 'expectedVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjeExpectedVersion :: Lens.Lens' CancelJobExecution (Lude.Maybe Lude.Integer)
cjeExpectedVersion = Lens.lens (expectedVersion :: CancelJobExecution -> Lude.Maybe Lude.Integer) (\s a -> s {expectedVersion = a} :: CancelJobExecution)
{-# DEPRECATED cjeExpectedVersion "Use generic-lens or generic-optics with 'expectedVersion' instead." #-}

-- | The name of the thing whose execution of the job will be canceled.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjeThingName :: Lens.Lens' CancelJobExecution Lude.Text
cjeThingName = Lens.lens (thingName :: CancelJobExecution -> Lude.Text) (\s a -> s {thingName = a} :: CancelJobExecution)
{-# DEPRECATED cjeThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Lude.AWSRequest CancelJobExecution where
  type Rs CancelJobExecution = CancelJobExecutionResponse
  request = Req.putJSON ioTService
  response = Res.receiveNull CancelJobExecutionResponse'

instance Lude.ToHeaders CancelJobExecution where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CancelJobExecution where
  toJSON CancelJobExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("statusDetails" Lude..=) Lude.<$> statusDetails,
            ("expectedVersion" Lude..=) Lude.<$> expectedVersion
          ]
      )

instance Lude.ToPath CancelJobExecution where
  toPath CancelJobExecution' {..} =
    Lude.mconcat
      [ "/things/",
        Lude.toBS thingName,
        "/jobs/",
        Lude.toBS jobId,
        "/cancel"
      ]

instance Lude.ToQuery CancelJobExecution where
  toQuery CancelJobExecution' {..} =
    Lude.mconcat ["force" Lude.=: force]

-- | /See:/ 'mkCancelJobExecutionResponse' smart constructor.
data CancelJobExecutionResponse = CancelJobExecutionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelJobExecutionResponse' with the minimum fields required to make a request.
mkCancelJobExecutionResponse ::
  CancelJobExecutionResponse
mkCancelJobExecutionResponse = CancelJobExecutionResponse'

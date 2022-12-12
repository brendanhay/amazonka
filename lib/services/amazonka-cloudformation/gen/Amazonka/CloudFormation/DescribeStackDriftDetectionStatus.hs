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
-- Module      : Amazonka.CloudFormation.DescribeStackDriftDetectionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a stack drift detection operation. A stack
-- drift detection operation detects whether a stack\'s actual
-- configuration differs, or has /drifted/, from it\'s expected
-- configuration, as defined in the stack template and any values specified
-- as template parameters. A stack is considered to have drifted if one or
-- more of its resources have drifted. For more information about stack and
-- resource drift, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- Use DetectStackDrift to initiate a stack drift detection operation.
-- @DetectStackDrift@ returns a @StackDriftDetectionId@ you can use to
-- monitor the progress of the operation using
-- @DescribeStackDriftDetectionStatus@. Once the drift detection operation
-- has completed, use DescribeStackResourceDrifts to return drift
-- information about the stack and its resources.
module Amazonka.CloudFormation.DescribeStackDriftDetectionStatus
  ( -- * Creating a Request
    DescribeStackDriftDetectionStatus (..),
    newDescribeStackDriftDetectionStatus,

    -- * Request Lenses
    describeStackDriftDetectionStatus_stackDriftDetectionId,

    -- * Destructuring the Response
    DescribeStackDriftDetectionStatusResponse (..),
    newDescribeStackDriftDetectionStatusResponse,

    -- * Response Lenses
    describeStackDriftDetectionStatusResponse_detectionStatusReason,
    describeStackDriftDetectionStatusResponse_driftedStackResourceCount,
    describeStackDriftDetectionStatusResponse_stackDriftStatus,
    describeStackDriftDetectionStatusResponse_httpStatus,
    describeStackDriftDetectionStatusResponse_stackId,
    describeStackDriftDetectionStatusResponse_stackDriftDetectionId,
    describeStackDriftDetectionStatusResponse_detectionStatus,
    describeStackDriftDetectionStatusResponse_timestamp,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStackDriftDetectionStatus' smart constructor.
data DescribeStackDriftDetectionStatus = DescribeStackDriftDetectionStatus'
  { -- | The ID of the drift detection results of this operation.
    --
    -- CloudFormation generates new results, with a new drift detection ID,
    -- each time this operation is run. However, the number of drift results
    -- CloudFormation retains for any given stack, and for how long, may vary.
    stackDriftDetectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackDriftDetectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackDriftDetectionId', 'describeStackDriftDetectionStatus_stackDriftDetectionId' - The ID of the drift detection results of this operation.
--
-- CloudFormation generates new results, with a new drift detection ID,
-- each time this operation is run. However, the number of drift results
-- CloudFormation retains for any given stack, and for how long, may vary.
newDescribeStackDriftDetectionStatus ::
  -- | 'stackDriftDetectionId'
  Prelude.Text ->
  DescribeStackDriftDetectionStatus
newDescribeStackDriftDetectionStatus
  pStackDriftDetectionId_ =
    DescribeStackDriftDetectionStatus'
      { stackDriftDetectionId =
          pStackDriftDetectionId_
      }

-- | The ID of the drift detection results of this operation.
--
-- CloudFormation generates new results, with a new drift detection ID,
-- each time this operation is run. However, the number of drift results
-- CloudFormation retains for any given stack, and for how long, may vary.
describeStackDriftDetectionStatus_stackDriftDetectionId :: Lens.Lens' DescribeStackDriftDetectionStatus Prelude.Text
describeStackDriftDetectionStatus_stackDriftDetectionId = Lens.lens (\DescribeStackDriftDetectionStatus' {stackDriftDetectionId} -> stackDriftDetectionId) (\s@DescribeStackDriftDetectionStatus' {} a -> s {stackDriftDetectionId = a} :: DescribeStackDriftDetectionStatus)

instance
  Core.AWSRequest
    DescribeStackDriftDetectionStatus
  where
  type
    AWSResponse DescribeStackDriftDetectionStatus =
      DescribeStackDriftDetectionStatusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeStackDriftDetectionStatusResult"
      ( \s h x ->
          DescribeStackDriftDetectionStatusResponse'
            Prelude.<$> (x Data..@? "DetectionStatusReason")
              Prelude.<*> (x Data..@? "DriftedStackResourceCount")
              Prelude.<*> (x Data..@? "StackDriftStatus")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..@ "StackId")
              Prelude.<*> (x Data..@ "StackDriftDetectionId")
              Prelude.<*> (x Data..@ "DetectionStatus")
              Prelude.<*> (x Data..@ "Timestamp")
      )

instance
  Prelude.Hashable
    DescribeStackDriftDetectionStatus
  where
  hashWithSalt
    _salt
    DescribeStackDriftDetectionStatus' {..} =
      _salt `Prelude.hashWithSalt` stackDriftDetectionId

instance
  Prelude.NFData
    DescribeStackDriftDetectionStatus
  where
  rnf DescribeStackDriftDetectionStatus' {..} =
    Prelude.rnf stackDriftDetectionId

instance
  Data.ToHeaders
    DescribeStackDriftDetectionStatus
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DescribeStackDriftDetectionStatus
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeStackDriftDetectionStatus
  where
  toQuery DescribeStackDriftDetectionStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeStackDriftDetectionStatus" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "StackDriftDetectionId"
          Data.=: stackDriftDetectionId
      ]

-- | /See:/ 'newDescribeStackDriftDetectionStatusResponse' smart constructor.
data DescribeStackDriftDetectionStatusResponse = DescribeStackDriftDetectionStatusResponse'
  { -- | The reason the stack drift detection operation has its current status.
    detectionStatusReason :: Prelude.Maybe Prelude.Text,
    -- | Total number of stack resources that have drifted. This is NULL until
    -- the drift detection operation reaches a status of @DETECTION_COMPLETE@.
    -- This value will be 0 for stacks whose drift status is @IN_SYNC@.
    driftedStackResourceCount :: Prelude.Maybe Prelude.Int,
    -- | Status of the stack\'s actual configuration compared to its expected
    -- configuration.
    --
    -- -   @DRIFTED@: The stack differs from its expected template
    --     configuration. A stack is considered to have drifted if one or more
    --     of its resources have drifted.
    --
    -- -   @NOT_CHECKED@: CloudFormation hasn\'t checked if the stack differs
    --     from its expected template configuration.
    --
    -- -   @IN_SYNC@: The stack\'s actual configuration matches its expected
    --     template configuration.
    --
    -- -   @UNKNOWN@: This value is reserved for future use.
    stackDriftStatus :: Prelude.Maybe StackDriftStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the stack.
    stackId :: Prelude.Text,
    -- | The ID of the drift detection results of this operation.
    --
    -- CloudFormation generates new results, with a new drift detection ID,
    -- each time this operation is run. However, the number of reports
    -- CloudFormation retains for any given stack, and for how long, may vary.
    stackDriftDetectionId :: Prelude.Text,
    -- | The status of the stack drift detection operation.
    --
    -- -   @DETECTION_COMPLETE@: The stack drift detection operation has
    --     successfully completed for all resources in the stack that support
    --     drift detection. (Resources that don\'t currently support stack
    --     detection remain unchecked.)
    --
    --     If you specified logical resource IDs for CloudFormation to use as a
    --     filter for the stack drift detection operation, only the resources
    --     with those logical IDs are checked for drift.
    --
    -- -   @DETECTION_FAILED@: The stack drift detection operation has failed
    --     for at least one resource in the stack. Results will be available
    --     for resources on which CloudFormation successfully completed drift
    --     detection.
    --
    -- -   @DETECTION_IN_PROGRESS@: The stack drift detection operation is
    --     currently in progress.
    detectionStatus :: StackDriftDetectionStatus,
    -- | Time at which the stack drift detection operation was initiated.
    timestamp :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackDriftDetectionStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectionStatusReason', 'describeStackDriftDetectionStatusResponse_detectionStatusReason' - The reason the stack drift detection operation has its current status.
--
-- 'driftedStackResourceCount', 'describeStackDriftDetectionStatusResponse_driftedStackResourceCount' - Total number of stack resources that have drifted. This is NULL until
-- the drift detection operation reaches a status of @DETECTION_COMPLETE@.
-- This value will be 0 for stacks whose drift status is @IN_SYNC@.
--
-- 'stackDriftStatus', 'describeStackDriftDetectionStatusResponse_stackDriftStatus' - Status of the stack\'s actual configuration compared to its expected
-- configuration.
--
-- -   @DRIFTED@: The stack differs from its expected template
--     configuration. A stack is considered to have drifted if one or more
--     of its resources have drifted.
--
-- -   @NOT_CHECKED@: CloudFormation hasn\'t checked if the stack differs
--     from its expected template configuration.
--
-- -   @IN_SYNC@: The stack\'s actual configuration matches its expected
--     template configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
--
-- 'httpStatus', 'describeStackDriftDetectionStatusResponse_httpStatus' - The response's http status code.
--
-- 'stackId', 'describeStackDriftDetectionStatusResponse_stackId' - The ID of the stack.
--
-- 'stackDriftDetectionId', 'describeStackDriftDetectionStatusResponse_stackDriftDetectionId' - The ID of the drift detection results of this operation.
--
-- CloudFormation generates new results, with a new drift detection ID,
-- each time this operation is run. However, the number of reports
-- CloudFormation retains for any given stack, and for how long, may vary.
--
-- 'detectionStatus', 'describeStackDriftDetectionStatusResponse_detectionStatus' - The status of the stack drift detection operation.
--
-- -   @DETECTION_COMPLETE@: The stack drift detection operation has
--     successfully completed for all resources in the stack that support
--     drift detection. (Resources that don\'t currently support stack
--     detection remain unchecked.)
--
--     If you specified logical resource IDs for CloudFormation to use as a
--     filter for the stack drift detection operation, only the resources
--     with those logical IDs are checked for drift.
--
-- -   @DETECTION_FAILED@: The stack drift detection operation has failed
--     for at least one resource in the stack. Results will be available
--     for resources on which CloudFormation successfully completed drift
--     detection.
--
-- -   @DETECTION_IN_PROGRESS@: The stack drift detection operation is
--     currently in progress.
--
-- 'timestamp', 'describeStackDriftDetectionStatusResponse_timestamp' - Time at which the stack drift detection operation was initiated.
newDescribeStackDriftDetectionStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'stackId'
  Prelude.Text ->
  -- | 'stackDriftDetectionId'
  Prelude.Text ->
  -- | 'detectionStatus'
  StackDriftDetectionStatus ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  DescribeStackDriftDetectionStatusResponse
newDescribeStackDriftDetectionStatusResponse
  pHttpStatus_
  pStackId_
  pStackDriftDetectionId_
  pDetectionStatus_
  pTimestamp_ =
    DescribeStackDriftDetectionStatusResponse'
      { detectionStatusReason =
          Prelude.Nothing,
        driftedStackResourceCount =
          Prelude.Nothing,
        stackDriftStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        stackId = pStackId_,
        stackDriftDetectionId =
          pStackDriftDetectionId_,
        detectionStatus =
          pDetectionStatus_,
        timestamp =
          Data._Time Lens.# pTimestamp_
      }

-- | The reason the stack drift detection operation has its current status.
describeStackDriftDetectionStatusResponse_detectionStatusReason :: Lens.Lens' DescribeStackDriftDetectionStatusResponse (Prelude.Maybe Prelude.Text)
describeStackDriftDetectionStatusResponse_detectionStatusReason = Lens.lens (\DescribeStackDriftDetectionStatusResponse' {detectionStatusReason} -> detectionStatusReason) (\s@DescribeStackDriftDetectionStatusResponse' {} a -> s {detectionStatusReason = a} :: DescribeStackDriftDetectionStatusResponse)

-- | Total number of stack resources that have drifted. This is NULL until
-- the drift detection operation reaches a status of @DETECTION_COMPLETE@.
-- This value will be 0 for stacks whose drift status is @IN_SYNC@.
describeStackDriftDetectionStatusResponse_driftedStackResourceCount :: Lens.Lens' DescribeStackDriftDetectionStatusResponse (Prelude.Maybe Prelude.Int)
describeStackDriftDetectionStatusResponse_driftedStackResourceCount = Lens.lens (\DescribeStackDriftDetectionStatusResponse' {driftedStackResourceCount} -> driftedStackResourceCount) (\s@DescribeStackDriftDetectionStatusResponse' {} a -> s {driftedStackResourceCount = a} :: DescribeStackDriftDetectionStatusResponse)

-- | Status of the stack\'s actual configuration compared to its expected
-- configuration.
--
-- -   @DRIFTED@: The stack differs from its expected template
--     configuration. A stack is considered to have drifted if one or more
--     of its resources have drifted.
--
-- -   @NOT_CHECKED@: CloudFormation hasn\'t checked if the stack differs
--     from its expected template configuration.
--
-- -   @IN_SYNC@: The stack\'s actual configuration matches its expected
--     template configuration.
--
-- -   @UNKNOWN@: This value is reserved for future use.
describeStackDriftDetectionStatusResponse_stackDriftStatus :: Lens.Lens' DescribeStackDriftDetectionStatusResponse (Prelude.Maybe StackDriftStatus)
describeStackDriftDetectionStatusResponse_stackDriftStatus = Lens.lens (\DescribeStackDriftDetectionStatusResponse' {stackDriftStatus} -> stackDriftStatus) (\s@DescribeStackDriftDetectionStatusResponse' {} a -> s {stackDriftStatus = a} :: DescribeStackDriftDetectionStatusResponse)

-- | The response's http status code.
describeStackDriftDetectionStatusResponse_httpStatus :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Prelude.Int
describeStackDriftDetectionStatusResponse_httpStatus = Lens.lens (\DescribeStackDriftDetectionStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeStackDriftDetectionStatusResponse' {} a -> s {httpStatus = a} :: DescribeStackDriftDetectionStatusResponse)

-- | The ID of the stack.
describeStackDriftDetectionStatusResponse_stackId :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Prelude.Text
describeStackDriftDetectionStatusResponse_stackId = Lens.lens (\DescribeStackDriftDetectionStatusResponse' {stackId} -> stackId) (\s@DescribeStackDriftDetectionStatusResponse' {} a -> s {stackId = a} :: DescribeStackDriftDetectionStatusResponse)

-- | The ID of the drift detection results of this operation.
--
-- CloudFormation generates new results, with a new drift detection ID,
-- each time this operation is run. However, the number of reports
-- CloudFormation retains for any given stack, and for how long, may vary.
describeStackDriftDetectionStatusResponse_stackDriftDetectionId :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Prelude.Text
describeStackDriftDetectionStatusResponse_stackDriftDetectionId = Lens.lens (\DescribeStackDriftDetectionStatusResponse' {stackDriftDetectionId} -> stackDriftDetectionId) (\s@DescribeStackDriftDetectionStatusResponse' {} a -> s {stackDriftDetectionId = a} :: DescribeStackDriftDetectionStatusResponse)

-- | The status of the stack drift detection operation.
--
-- -   @DETECTION_COMPLETE@: The stack drift detection operation has
--     successfully completed for all resources in the stack that support
--     drift detection. (Resources that don\'t currently support stack
--     detection remain unchecked.)
--
--     If you specified logical resource IDs for CloudFormation to use as a
--     filter for the stack drift detection operation, only the resources
--     with those logical IDs are checked for drift.
--
-- -   @DETECTION_FAILED@: The stack drift detection operation has failed
--     for at least one resource in the stack. Results will be available
--     for resources on which CloudFormation successfully completed drift
--     detection.
--
-- -   @DETECTION_IN_PROGRESS@: The stack drift detection operation is
--     currently in progress.
describeStackDriftDetectionStatusResponse_detectionStatus :: Lens.Lens' DescribeStackDriftDetectionStatusResponse StackDriftDetectionStatus
describeStackDriftDetectionStatusResponse_detectionStatus = Lens.lens (\DescribeStackDriftDetectionStatusResponse' {detectionStatus} -> detectionStatus) (\s@DescribeStackDriftDetectionStatusResponse' {} a -> s {detectionStatus = a} :: DescribeStackDriftDetectionStatusResponse)

-- | Time at which the stack drift detection operation was initiated.
describeStackDriftDetectionStatusResponse_timestamp :: Lens.Lens' DescribeStackDriftDetectionStatusResponse Prelude.UTCTime
describeStackDriftDetectionStatusResponse_timestamp = Lens.lens (\DescribeStackDriftDetectionStatusResponse' {timestamp} -> timestamp) (\s@DescribeStackDriftDetectionStatusResponse' {} a -> s {timestamp = a} :: DescribeStackDriftDetectionStatusResponse) Prelude.. Data._Time

instance
  Prelude.NFData
    DescribeStackDriftDetectionStatusResponse
  where
  rnf DescribeStackDriftDetectionStatusResponse' {..} =
    Prelude.rnf detectionStatusReason
      `Prelude.seq` Prelude.rnf driftedStackResourceCount
      `Prelude.seq` Prelude.rnf stackDriftStatus
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf stackDriftDetectionId
      `Prelude.seq` Prelude.rnf detectionStatus
      `Prelude.seq` Prelude.rnf timestamp

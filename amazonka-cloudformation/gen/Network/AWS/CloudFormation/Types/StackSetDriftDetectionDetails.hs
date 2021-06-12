{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails where

import Network.AWS.CloudFormation.Types.StackSetDriftDetectionStatus
import Network.AWS.CloudFormation.Types.StackSetDriftStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift
-- operation performed on the stack set. Information about drift operations
-- in-progress is not included.
--
-- For stack set operations, includes information about drift operations
-- currently being performed on the stack set.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets>
-- in the /AWS CloudFormation User Guide/.
--
-- /See:/ 'newStackSetDriftDetectionDetails' smart constructor.
data StackSetDriftDetectionDetails = StackSetDriftDetectionDetails'
  { -- | The number of stack instances which match the expected template and
    -- parameter configuration of the stack set.
    inSyncStackInstancesCount :: Core.Maybe Core.Natural,
    -- | The number of stack instances for which the drift detection operation
    -- failed.
    failedStackInstancesCount :: Core.Maybe Core.Natural,
    -- | The number of stack instances that have drifted from the expected
    -- template and parameter configuration of the stack set. A stack instance
    -- is considered to have drifted if one or more of the resources in the
    -- associated stack do not match their expected configuration.
    driftedStackInstancesCount :: Core.Maybe Core.Natural,
    -- | The number of stack instances that are currently being checked for
    -- drift.
    inProgressStackInstancesCount :: Core.Maybe Core.Natural,
    -- | Most recent time when CloudFormation performed a drift detection
    -- operation on the stack set. This value will be @NULL@ for any stack set
    -- on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Core.Maybe Core.ISO8601,
    -- | Status of the stack set\'s actual configuration compared to its expected
    -- template and parameter configuration. A stack set is considered to have
    -- drifted if one or more of its stack instances have drifted from their
    -- expected template and parameter configuration.
    --
    -- -   @DRIFTED@: One or more of the stack instances belonging to the stack
    --     set stack differs from the expected template and parameter
    --     configuration. A stack instance is considered to have drifted if one
    --     or more of the resources in the associated stack have drifted.
    --
    -- -   @NOT_CHECKED@: AWS CloudFormation has not checked the stack set for
    --     drift.
    --
    -- -   @IN_SYNC@: All of the stack instances belonging to the stack set
    --     stack match from the expected template and parameter configuration.
    driftStatus :: Core.Maybe StackSetDriftStatus,
    -- | The status of the stack set drift detection operation.
    --
    -- -   @COMPLETED@: The drift detection operation completed without failing
    --     on any stack instances.
    --
    -- -   @FAILED@: The drift detection operation exceeded the specified
    --     failure tolerance.
    --
    -- -   @PARTIAL_SUCCESS@: The drift detection operation completed without
    --     exceeding the failure tolerance for the operation.
    --
    -- -   @IN_PROGRESS@: The drift detection operation is currently being
    --     performed.
    --
    -- -   @STOPPED@: The user has cancelled the drift detection operation.
    driftDetectionStatus :: Core.Maybe StackSetDriftDetectionStatus,
    -- | The total number of stack instances belonging to this stack set.
    --
    -- The total number of stack instances is equal to the total of:
    --
    -- -   Stack instances that match the stack set configuration.
    --
    -- -   Stack instances that have drifted from the stack set configuration.
    --
    -- -   Stack instances where the drift detection operation has failed.
    --
    -- -   Stack instances currently being checked for drift.
    totalStackInstancesCount :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StackSetDriftDetectionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inSyncStackInstancesCount', 'stackSetDriftDetectionDetails_inSyncStackInstancesCount' - The number of stack instances which match the expected template and
-- parameter configuration of the stack set.
--
-- 'failedStackInstancesCount', 'stackSetDriftDetectionDetails_failedStackInstancesCount' - The number of stack instances for which the drift detection operation
-- failed.
--
-- 'driftedStackInstancesCount', 'stackSetDriftDetectionDetails_driftedStackInstancesCount' - The number of stack instances that have drifted from the expected
-- template and parameter configuration of the stack set. A stack instance
-- is considered to have drifted if one or more of the resources in the
-- associated stack do not match their expected configuration.
--
-- 'inProgressStackInstancesCount', 'stackSetDriftDetectionDetails_inProgressStackInstancesCount' - The number of stack instances that are currently being checked for
-- drift.
--
-- 'lastDriftCheckTimestamp', 'stackSetDriftDetectionDetails_lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection has not yet been performed.
--
-- 'driftStatus', 'stackSetDriftDetectionDetails_driftStatus' - Status of the stack set\'s actual configuration compared to its expected
-- template and parameter configuration. A stack set is considered to have
-- drifted if one or more of its stack instances have drifted from their
-- expected template and parameter configuration.
--
-- -   @DRIFTED@: One or more of the stack instances belonging to the stack
--     set stack differs from the expected template and parameter
--     configuration. A stack instance is considered to have drifted if one
--     or more of the resources in the associated stack have drifted.
--
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked the stack set for
--     drift.
--
-- -   @IN_SYNC@: All of the stack instances belonging to the stack set
--     stack match from the expected template and parameter configuration.
--
-- 'driftDetectionStatus', 'stackSetDriftDetectionDetails_driftDetectionStatus' - The status of the stack set drift detection operation.
--
-- -   @COMPLETED@: The drift detection operation completed without failing
--     on any stack instances.
--
-- -   @FAILED@: The drift detection operation exceeded the specified
--     failure tolerance.
--
-- -   @PARTIAL_SUCCESS@: The drift detection operation completed without
--     exceeding the failure tolerance for the operation.
--
-- -   @IN_PROGRESS@: The drift detection operation is currently being
--     performed.
--
-- -   @STOPPED@: The user has cancelled the drift detection operation.
--
-- 'totalStackInstancesCount', 'stackSetDriftDetectionDetails_totalStackInstancesCount' - The total number of stack instances belonging to this stack set.
--
-- The total number of stack instances is equal to the total of:
--
-- -   Stack instances that match the stack set configuration.
--
-- -   Stack instances that have drifted from the stack set configuration.
--
-- -   Stack instances where the drift detection operation has failed.
--
-- -   Stack instances currently being checked for drift.
newStackSetDriftDetectionDetails ::
  StackSetDriftDetectionDetails
newStackSetDriftDetectionDetails =
  StackSetDriftDetectionDetails'
    { inSyncStackInstancesCount =
        Core.Nothing,
      failedStackInstancesCount = Core.Nothing,
      driftedStackInstancesCount = Core.Nothing,
      inProgressStackInstancesCount = Core.Nothing,
      lastDriftCheckTimestamp = Core.Nothing,
      driftStatus = Core.Nothing,
      driftDetectionStatus = Core.Nothing,
      totalStackInstancesCount = Core.Nothing
    }

-- | The number of stack instances which match the expected template and
-- parameter configuration of the stack set.
stackSetDriftDetectionDetails_inSyncStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.Natural)
stackSetDriftDetectionDetails_inSyncStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {inSyncStackInstancesCount} -> inSyncStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {inSyncStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances for which the drift detection operation
-- failed.
stackSetDriftDetectionDetails_failedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.Natural)
stackSetDriftDetectionDetails_failedStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {failedStackInstancesCount} -> failedStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {failedStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances that have drifted from the expected
-- template and parameter configuration of the stack set. A stack instance
-- is considered to have drifted if one or more of the resources in the
-- associated stack do not match their expected configuration.
stackSetDriftDetectionDetails_driftedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.Natural)
stackSetDriftDetectionDetails_driftedStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {driftedStackInstancesCount} -> driftedStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {driftedStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances that are currently being checked for
-- drift.
stackSetDriftDetectionDetails_inProgressStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.Natural)
stackSetDriftDetectionDetails_inProgressStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {inProgressStackInstancesCount} -> inProgressStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {inProgressStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection has not yet been performed.
stackSetDriftDetectionDetails_lastDriftCheckTimestamp :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.UTCTime)
stackSetDriftDetectionDetails_lastDriftCheckTimestamp = Lens.lens (\StackSetDriftDetectionDetails' {lastDriftCheckTimestamp} -> lastDriftCheckTimestamp) (\s@StackSetDriftDetectionDetails' {} a -> s {lastDriftCheckTimestamp = a} :: StackSetDriftDetectionDetails) Core.. Lens.mapping Core._Time

-- | Status of the stack set\'s actual configuration compared to its expected
-- template and parameter configuration. A stack set is considered to have
-- drifted if one or more of its stack instances have drifted from their
-- expected template and parameter configuration.
--
-- -   @DRIFTED@: One or more of the stack instances belonging to the stack
--     set stack differs from the expected template and parameter
--     configuration. A stack instance is considered to have drifted if one
--     or more of the resources in the associated stack have drifted.
--
-- -   @NOT_CHECKED@: AWS CloudFormation has not checked the stack set for
--     drift.
--
-- -   @IN_SYNC@: All of the stack instances belonging to the stack set
--     stack match from the expected template and parameter configuration.
stackSetDriftDetectionDetails_driftStatus :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe StackSetDriftStatus)
stackSetDriftDetectionDetails_driftStatus = Lens.lens (\StackSetDriftDetectionDetails' {driftStatus} -> driftStatus) (\s@StackSetDriftDetectionDetails' {} a -> s {driftStatus = a} :: StackSetDriftDetectionDetails)

-- | The status of the stack set drift detection operation.
--
-- -   @COMPLETED@: The drift detection operation completed without failing
--     on any stack instances.
--
-- -   @FAILED@: The drift detection operation exceeded the specified
--     failure tolerance.
--
-- -   @PARTIAL_SUCCESS@: The drift detection operation completed without
--     exceeding the failure tolerance for the operation.
--
-- -   @IN_PROGRESS@: The drift detection operation is currently being
--     performed.
--
-- -   @STOPPED@: The user has cancelled the drift detection operation.
stackSetDriftDetectionDetails_driftDetectionStatus :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe StackSetDriftDetectionStatus)
stackSetDriftDetectionDetails_driftDetectionStatus = Lens.lens (\StackSetDriftDetectionDetails' {driftDetectionStatus} -> driftDetectionStatus) (\s@StackSetDriftDetectionDetails' {} a -> s {driftDetectionStatus = a} :: StackSetDriftDetectionDetails)

-- | The total number of stack instances belonging to this stack set.
--
-- The total number of stack instances is equal to the total of:
--
-- -   Stack instances that match the stack set configuration.
--
-- -   Stack instances that have drifted from the stack set configuration.
--
-- -   Stack instances where the drift detection operation has failed.
--
-- -   Stack instances currently being checked for drift.
stackSetDriftDetectionDetails_totalStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Core.Maybe Core.Natural)
stackSetDriftDetectionDetails_totalStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {totalStackInstancesCount} -> totalStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {totalStackInstancesCount = a} :: StackSetDriftDetectionDetails)

instance Core.FromXML StackSetDriftDetectionDetails where
  parseXML x =
    StackSetDriftDetectionDetails'
      Core.<$> (x Core..@? "InSyncStackInstancesCount")
      Core.<*> (x Core..@? "FailedStackInstancesCount")
      Core.<*> (x Core..@? "DriftedStackInstancesCount")
      Core.<*> (x Core..@? "InProgressStackInstancesCount")
      Core.<*> (x Core..@? "LastDriftCheckTimestamp")
      Core.<*> (x Core..@? "DriftStatus")
      Core.<*> (x Core..@? "DriftDetectionStatus")
      Core.<*> (x Core..@? "TotalStackInstancesCount")

instance Core.Hashable StackSetDriftDetectionDetails

instance Core.NFData StackSetDriftDetectionDetails

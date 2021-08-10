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
import qualified Network.AWS.Prelude as Prelude

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
    inSyncStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of stack instances for which the drift detection operation
    -- failed.
    failedStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of stack instances that have drifted from the expected
    -- template and parameter configuration of the stack set. A stack instance
    -- is considered to have drifted if one or more of the resources in the
    -- associated stack do not match their expected configuration.
    driftedStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of stack instances that are currently being checked for
    -- drift.
    inProgressStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | Most recent time when CloudFormation performed a drift detection
    -- operation on the stack set. This value will be @NULL@ for any stack set
    -- on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Prelude.Maybe Core.ISO8601,
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
    driftStatus :: Prelude.Maybe StackSetDriftStatus,
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
    driftDetectionStatus :: Prelude.Maybe StackSetDriftDetectionStatus,
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
    totalStackInstancesCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      failedStackInstancesCount = Prelude.Nothing,
      driftedStackInstancesCount = Prelude.Nothing,
      inProgressStackInstancesCount =
        Prelude.Nothing,
      lastDriftCheckTimestamp = Prelude.Nothing,
      driftStatus = Prelude.Nothing,
      driftDetectionStatus = Prelude.Nothing,
      totalStackInstancesCount = Prelude.Nothing
    }

-- | The number of stack instances which match the expected template and
-- parameter configuration of the stack set.
stackSetDriftDetectionDetails_inSyncStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_inSyncStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {inSyncStackInstancesCount} -> inSyncStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {inSyncStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances for which the drift detection operation
-- failed.
stackSetDriftDetectionDetails_failedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_failedStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {failedStackInstancesCount} -> failedStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {failedStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances that have drifted from the expected
-- template and parameter configuration of the stack set. A stack instance
-- is considered to have drifted if one or more of the resources in the
-- associated stack do not match their expected configuration.
stackSetDriftDetectionDetails_driftedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_driftedStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {driftedStackInstancesCount} -> driftedStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {driftedStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances that are currently being checked for
-- drift.
stackSetDriftDetectionDetails_inProgressStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_inProgressStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {inProgressStackInstancesCount} -> inProgressStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {inProgressStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection has not yet been performed.
stackSetDriftDetectionDetails_lastDriftCheckTimestamp :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.UTCTime)
stackSetDriftDetectionDetails_lastDriftCheckTimestamp = Lens.lens (\StackSetDriftDetectionDetails' {lastDriftCheckTimestamp} -> lastDriftCheckTimestamp) (\s@StackSetDriftDetectionDetails' {} a -> s {lastDriftCheckTimestamp = a} :: StackSetDriftDetectionDetails) Prelude.. Lens.mapping Core._Time

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
stackSetDriftDetectionDetails_driftStatus :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe StackSetDriftStatus)
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
stackSetDriftDetectionDetails_driftDetectionStatus :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe StackSetDriftDetectionStatus)
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
stackSetDriftDetectionDetails_totalStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_totalStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {totalStackInstancesCount} -> totalStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {totalStackInstancesCount = a} :: StackSetDriftDetectionDetails)

instance Core.FromXML StackSetDriftDetectionDetails where
  parseXML x =
    StackSetDriftDetectionDetails'
      Prelude.<$> (x Core..@? "InSyncStackInstancesCount")
      Prelude.<*> (x Core..@? "FailedStackInstancesCount")
      Prelude.<*> (x Core..@? "DriftedStackInstancesCount")
      Prelude.<*> (x Core..@? "InProgressStackInstancesCount")
      Prelude.<*> (x Core..@? "LastDriftCheckTimestamp")
      Prelude.<*> (x Core..@? "DriftStatus")
      Prelude.<*> (x Core..@? "DriftDetectionStatus")
      Prelude.<*> (x Core..@? "TotalStackInstancesCount")

instance
  Prelude.Hashable
    StackSetDriftDetectionDetails

instance Prelude.NFData StackSetDriftDetectionDetails

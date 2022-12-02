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
-- Module      : Amazonka.CloudFormation.Types.StackSetDriftDetectionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetDriftDetectionDetails where

import Amazonka.CloudFormation.Types.StackSetDriftDetectionStatus
import Amazonka.CloudFormation.Types.StackSetDriftStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift
-- operation performed on the stack set. Information about drift operations
-- in-progress isn\'t included.
--
-- For stack set operations, includes information about drift operations
-- currently being performed on the stack set.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting unmanaged changes in stack sets>
-- in the /CloudFormation User Guide/.
--
-- /See:/ 'newStackSetDriftDetectionDetails' smart constructor.
data StackSetDriftDetectionDetails = StackSetDriftDetectionDetails'
  { -- | The number of stack instances that are currently being checked for
    -- drift.
    inProgressStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of stack instances for which the drift detection operation
    -- failed.
    failedStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of stack instances that have drifted from the expected
    -- template and parameter configuration of the stack set. A stack instance
    -- is considered to have drifted if one or more of the resources in the
    -- associated stack don\'t match their expected configuration.
    driftedStackInstancesCount :: Prelude.Maybe Prelude.Natural,
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
    -- -   @STOPPED@: The user has canceled the drift detection operation.
    driftDetectionStatus :: Prelude.Maybe StackSetDriftDetectionStatus,
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
    -- -   @NOT_CHECKED@: CloudFormation hasn\'t checked the stack set for
    --     drift.
    --
    -- -   @IN_SYNC@: All of the stack instances belonging to the stack set
    --     stack match from the expected template and parameter configuration.
    driftStatus :: Prelude.Maybe StackSetDriftStatus,
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
    totalStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of stack instances which match the expected template and
    -- parameter configuration of the stack set.
    inSyncStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | Most recent time when CloudFormation performed a drift detection
    -- operation on the stack set. This value will be @NULL@ for any stack set
    -- on which drift detection hasn\'t yet been performed.
    lastDriftCheckTimestamp :: Prelude.Maybe Data.ISO8601
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
-- 'inProgressStackInstancesCount', 'stackSetDriftDetectionDetails_inProgressStackInstancesCount' - The number of stack instances that are currently being checked for
-- drift.
--
-- 'failedStackInstancesCount', 'stackSetDriftDetectionDetails_failedStackInstancesCount' - The number of stack instances for which the drift detection operation
-- failed.
--
-- 'driftedStackInstancesCount', 'stackSetDriftDetectionDetails_driftedStackInstancesCount' - The number of stack instances that have drifted from the expected
-- template and parameter configuration of the stack set. A stack instance
-- is considered to have drifted if one or more of the resources in the
-- associated stack don\'t match their expected configuration.
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
-- -   @STOPPED@: The user has canceled the drift detection operation.
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
-- -   @NOT_CHECKED@: CloudFormation hasn\'t checked the stack set for
--     drift.
--
-- -   @IN_SYNC@: All of the stack instances belonging to the stack set
--     stack match from the expected template and parameter configuration.
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
--
-- 'inSyncStackInstancesCount', 'stackSetDriftDetectionDetails_inSyncStackInstancesCount' - The number of stack instances which match the expected template and
-- parameter configuration of the stack set.
--
-- 'lastDriftCheckTimestamp', 'stackSetDriftDetectionDetails_lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection hasn\'t yet been performed.
newStackSetDriftDetectionDetails ::
  StackSetDriftDetectionDetails
newStackSetDriftDetectionDetails =
  StackSetDriftDetectionDetails'
    { inProgressStackInstancesCount =
        Prelude.Nothing,
      failedStackInstancesCount = Prelude.Nothing,
      driftedStackInstancesCount = Prelude.Nothing,
      driftDetectionStatus = Prelude.Nothing,
      driftStatus = Prelude.Nothing,
      totalStackInstancesCount = Prelude.Nothing,
      inSyncStackInstancesCount = Prelude.Nothing,
      lastDriftCheckTimestamp = Prelude.Nothing
    }

-- | The number of stack instances that are currently being checked for
-- drift.
stackSetDriftDetectionDetails_inProgressStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_inProgressStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {inProgressStackInstancesCount} -> inProgressStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {inProgressStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances for which the drift detection operation
-- failed.
stackSetDriftDetectionDetails_failedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_failedStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {failedStackInstancesCount} -> failedStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {failedStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances that have drifted from the expected
-- template and parameter configuration of the stack set. A stack instance
-- is considered to have drifted if one or more of the resources in the
-- associated stack don\'t match their expected configuration.
stackSetDriftDetectionDetails_driftedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_driftedStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {driftedStackInstancesCount} -> driftedStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {driftedStackInstancesCount = a} :: StackSetDriftDetectionDetails)

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
-- -   @STOPPED@: The user has canceled the drift detection operation.
stackSetDriftDetectionDetails_driftDetectionStatus :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe StackSetDriftDetectionStatus)
stackSetDriftDetectionDetails_driftDetectionStatus = Lens.lens (\StackSetDriftDetectionDetails' {driftDetectionStatus} -> driftDetectionStatus) (\s@StackSetDriftDetectionDetails' {} a -> s {driftDetectionStatus = a} :: StackSetDriftDetectionDetails)

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
-- -   @NOT_CHECKED@: CloudFormation hasn\'t checked the stack set for
--     drift.
--
-- -   @IN_SYNC@: All of the stack instances belonging to the stack set
--     stack match from the expected template and parameter configuration.
stackSetDriftDetectionDetails_driftStatus :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe StackSetDriftStatus)
stackSetDriftDetectionDetails_driftStatus = Lens.lens (\StackSetDriftDetectionDetails' {driftStatus} -> driftStatus) (\s@StackSetDriftDetectionDetails' {} a -> s {driftStatus = a} :: StackSetDriftDetectionDetails)

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

-- | The number of stack instances which match the expected template and
-- parameter configuration of the stack set.
stackSetDriftDetectionDetails_inSyncStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_inSyncStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {inSyncStackInstancesCount} -> inSyncStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {inSyncStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection hasn\'t yet been performed.
stackSetDriftDetectionDetails_lastDriftCheckTimestamp :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.UTCTime)
stackSetDriftDetectionDetails_lastDriftCheckTimestamp = Lens.lens (\StackSetDriftDetectionDetails' {lastDriftCheckTimestamp} -> lastDriftCheckTimestamp) (\s@StackSetDriftDetectionDetails' {} a -> s {lastDriftCheckTimestamp = a} :: StackSetDriftDetectionDetails) Prelude.. Lens.mapping Data._Time

instance Data.FromXML StackSetDriftDetectionDetails where
  parseXML x =
    StackSetDriftDetectionDetails'
      Prelude.<$> (x Data..@? "InProgressStackInstancesCount")
      Prelude.<*> (x Data..@? "FailedStackInstancesCount")
      Prelude.<*> (x Data..@? "DriftedStackInstancesCount")
      Prelude.<*> (x Data..@? "DriftDetectionStatus")
      Prelude.<*> (x Data..@? "DriftStatus")
      Prelude.<*> (x Data..@? "TotalStackInstancesCount")
      Prelude.<*> (x Data..@? "InSyncStackInstancesCount")
      Prelude.<*> (x Data..@? "LastDriftCheckTimestamp")

instance
  Prelude.Hashable
    StackSetDriftDetectionDetails
  where
  hashWithSalt _salt StackSetDriftDetectionDetails' {..} =
    _salt
      `Prelude.hashWithSalt` inProgressStackInstancesCount
      `Prelude.hashWithSalt` failedStackInstancesCount
      `Prelude.hashWithSalt` driftedStackInstancesCount
      `Prelude.hashWithSalt` driftDetectionStatus
      `Prelude.hashWithSalt` driftStatus
      `Prelude.hashWithSalt` totalStackInstancesCount
      `Prelude.hashWithSalt` inSyncStackInstancesCount
      `Prelude.hashWithSalt` lastDriftCheckTimestamp

instance Prelude.NFData StackSetDriftDetectionDetails where
  rnf StackSetDriftDetectionDetails' {..} =
    Prelude.rnf inProgressStackInstancesCount
      `Prelude.seq` Prelude.rnf failedStackInstancesCount
      `Prelude.seq` Prelude.rnf driftedStackInstancesCount
      `Prelude.seq` Prelude.rnf driftDetectionStatus
      `Prelude.seq` Prelude.rnf driftStatus
      `Prelude.seq` Prelude.rnf totalStackInstancesCount
      `Prelude.seq` Prelude.rnf inSyncStackInstancesCount
      `Prelude.seq` Prelude.rnf lastDriftCheckTimestamp

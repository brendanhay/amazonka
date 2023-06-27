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
-- Module      : Amazonka.AutoScaling.Types.InstanceRefresh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstanceRefresh where

import Amazonka.AutoScaling.Types.DesiredConfiguration
import Amazonka.AutoScaling.Types.InstanceRefreshProgressDetails
import Amazonka.AutoScaling.Types.InstanceRefreshStatus
import Amazonka.AutoScaling.Types.RefreshPreferences
import Amazonka.AutoScaling.Types.RollbackDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance refresh for an Auto Scaling group.
--
-- /See:/ 'newInstanceRefresh' smart constructor.
data InstanceRefresh = InstanceRefresh'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | Describes the desired configuration for the instance refresh.
    desiredConfiguration :: Prelude.Maybe DesiredConfiguration,
    -- | The date and time at which the instance refresh ended.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The instance refresh ID.
    instanceRefreshId :: Prelude.Maybe Prelude.Text,
    -- | The number of instances remaining to update before the instance refresh
    -- is complete.
    --
    -- If you roll back the instance refresh, @InstancesToUpdate@ shows you the
    -- number of instances that were not yet updated by the instance refresh.
    -- Therefore, these instances don\'t need to be replaced as part of the
    -- rollback.
    instancesToUpdate :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of the instance refresh that is complete. For each
    -- instance replacement, Amazon EC2 Auto Scaling tracks the instance\'s
    -- health status and warm-up time. When the instance\'s health status
    -- changes to healthy and the specified warm-up time passes, the instance
    -- is considered updated and is added to the percentage complete.
    --
    -- @PercentageComplete@ does not include instances that are replaced during
    -- a rollback. This value gradually goes back down to zero during a
    -- rollback.
    percentageComplete :: Prelude.Maybe Prelude.Natural,
    -- | The preferences for an instance refresh.
    preferences :: Prelude.Maybe RefreshPreferences,
    -- | Additional progress details for an Auto Scaling group that has a warm
    -- pool.
    progressDetails :: Prelude.Maybe InstanceRefreshProgressDetails,
    -- | The rollback details.
    rollbackDetails :: Prelude.Maybe RollbackDetails,
    -- | The date and time at which the instance refresh began.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The current status for the instance refresh operation:
    --
    -- -   @Pending@ - The request was created, but the instance refresh has
    --     not started.
    --
    -- -   @InProgress@ - An instance refresh is in progress.
    --
    -- -   @Successful@ - An instance refresh completed successfully.
    --
    -- -   @Failed@ - An instance refresh failed to complete. You can
    --     troubleshoot using the status reason and the scaling activities.
    --
    -- -   @Cancelling@ - An ongoing instance refresh is being cancelled.
    --
    -- -   @Cancelled@ - The instance refresh is cancelled.
    --
    -- -   @RollbackInProgress@ - An instance refresh is being rolled back.
    --
    -- -   @RollbackFailed@ - The rollback failed to complete. You can
    --     troubleshoot using the status reason and the scaling activities.
    --
    -- -   @RollbackSuccessful@ - The rollback completed successfully.
    status :: Prelude.Maybe InstanceRefreshStatus,
    -- | The explanation for the specific status assigned to this operation.
    statusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceRefresh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'instanceRefresh_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'desiredConfiguration', 'instanceRefresh_desiredConfiguration' - Describes the desired configuration for the instance refresh.
--
-- 'endTime', 'instanceRefresh_endTime' - The date and time at which the instance refresh ended.
--
-- 'instanceRefreshId', 'instanceRefresh_instanceRefreshId' - The instance refresh ID.
--
-- 'instancesToUpdate', 'instanceRefresh_instancesToUpdate' - The number of instances remaining to update before the instance refresh
-- is complete.
--
-- If you roll back the instance refresh, @InstancesToUpdate@ shows you the
-- number of instances that were not yet updated by the instance refresh.
-- Therefore, these instances don\'t need to be replaced as part of the
-- rollback.
--
-- 'percentageComplete', 'instanceRefresh_percentageComplete' - The percentage of the instance refresh that is complete. For each
-- instance replacement, Amazon EC2 Auto Scaling tracks the instance\'s
-- health status and warm-up time. When the instance\'s health status
-- changes to healthy and the specified warm-up time passes, the instance
-- is considered updated and is added to the percentage complete.
--
-- @PercentageComplete@ does not include instances that are replaced during
-- a rollback. This value gradually goes back down to zero during a
-- rollback.
--
-- 'preferences', 'instanceRefresh_preferences' - The preferences for an instance refresh.
--
-- 'progressDetails', 'instanceRefresh_progressDetails' - Additional progress details for an Auto Scaling group that has a warm
-- pool.
--
-- 'rollbackDetails', 'instanceRefresh_rollbackDetails' - The rollback details.
--
-- 'startTime', 'instanceRefresh_startTime' - The date and time at which the instance refresh began.
--
-- 'status', 'instanceRefresh_status' - The current status for the instance refresh operation:
--
-- -   @Pending@ - The request was created, but the instance refresh has
--     not started.
--
-- -   @InProgress@ - An instance refresh is in progress.
--
-- -   @Successful@ - An instance refresh completed successfully.
--
-- -   @Failed@ - An instance refresh failed to complete. You can
--     troubleshoot using the status reason and the scaling activities.
--
-- -   @Cancelling@ - An ongoing instance refresh is being cancelled.
--
-- -   @Cancelled@ - The instance refresh is cancelled.
--
-- -   @RollbackInProgress@ - An instance refresh is being rolled back.
--
-- -   @RollbackFailed@ - The rollback failed to complete. You can
--     troubleshoot using the status reason and the scaling activities.
--
-- -   @RollbackSuccessful@ - The rollback completed successfully.
--
-- 'statusReason', 'instanceRefresh_statusReason' - The explanation for the specific status assigned to this operation.
newInstanceRefresh ::
  InstanceRefresh
newInstanceRefresh =
  InstanceRefresh'
    { autoScalingGroupName =
        Prelude.Nothing,
      desiredConfiguration = Prelude.Nothing,
      endTime = Prelude.Nothing,
      instanceRefreshId = Prelude.Nothing,
      instancesToUpdate = Prelude.Nothing,
      percentageComplete = Prelude.Nothing,
      preferences = Prelude.Nothing,
      progressDetails = Prelude.Nothing,
      rollbackDetails = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The name of the Auto Scaling group.
instanceRefresh_autoScalingGroupName :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Text)
instanceRefresh_autoScalingGroupName = Lens.lens (\InstanceRefresh' {autoScalingGroupName} -> autoScalingGroupName) (\s@InstanceRefresh' {} a -> s {autoScalingGroupName = a} :: InstanceRefresh)

-- | Describes the desired configuration for the instance refresh.
instanceRefresh_desiredConfiguration :: Lens.Lens' InstanceRefresh (Prelude.Maybe DesiredConfiguration)
instanceRefresh_desiredConfiguration = Lens.lens (\InstanceRefresh' {desiredConfiguration} -> desiredConfiguration) (\s@InstanceRefresh' {} a -> s {desiredConfiguration = a} :: InstanceRefresh)

-- | The date and time at which the instance refresh ended.
instanceRefresh_endTime :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.UTCTime)
instanceRefresh_endTime = Lens.lens (\InstanceRefresh' {endTime} -> endTime) (\s@InstanceRefresh' {} a -> s {endTime = a} :: InstanceRefresh) Prelude.. Lens.mapping Data._Time

-- | The instance refresh ID.
instanceRefresh_instanceRefreshId :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Text)
instanceRefresh_instanceRefreshId = Lens.lens (\InstanceRefresh' {instanceRefreshId} -> instanceRefreshId) (\s@InstanceRefresh' {} a -> s {instanceRefreshId = a} :: InstanceRefresh)

-- | The number of instances remaining to update before the instance refresh
-- is complete.
--
-- If you roll back the instance refresh, @InstancesToUpdate@ shows you the
-- number of instances that were not yet updated by the instance refresh.
-- Therefore, these instances don\'t need to be replaced as part of the
-- rollback.
instanceRefresh_instancesToUpdate :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Natural)
instanceRefresh_instancesToUpdate = Lens.lens (\InstanceRefresh' {instancesToUpdate} -> instancesToUpdate) (\s@InstanceRefresh' {} a -> s {instancesToUpdate = a} :: InstanceRefresh)

-- | The percentage of the instance refresh that is complete. For each
-- instance replacement, Amazon EC2 Auto Scaling tracks the instance\'s
-- health status and warm-up time. When the instance\'s health status
-- changes to healthy and the specified warm-up time passes, the instance
-- is considered updated and is added to the percentage complete.
--
-- @PercentageComplete@ does not include instances that are replaced during
-- a rollback. This value gradually goes back down to zero during a
-- rollback.
instanceRefresh_percentageComplete :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Natural)
instanceRefresh_percentageComplete = Lens.lens (\InstanceRefresh' {percentageComplete} -> percentageComplete) (\s@InstanceRefresh' {} a -> s {percentageComplete = a} :: InstanceRefresh)

-- | The preferences for an instance refresh.
instanceRefresh_preferences :: Lens.Lens' InstanceRefresh (Prelude.Maybe RefreshPreferences)
instanceRefresh_preferences = Lens.lens (\InstanceRefresh' {preferences} -> preferences) (\s@InstanceRefresh' {} a -> s {preferences = a} :: InstanceRefresh)

-- | Additional progress details for an Auto Scaling group that has a warm
-- pool.
instanceRefresh_progressDetails :: Lens.Lens' InstanceRefresh (Prelude.Maybe InstanceRefreshProgressDetails)
instanceRefresh_progressDetails = Lens.lens (\InstanceRefresh' {progressDetails} -> progressDetails) (\s@InstanceRefresh' {} a -> s {progressDetails = a} :: InstanceRefresh)

-- | The rollback details.
instanceRefresh_rollbackDetails :: Lens.Lens' InstanceRefresh (Prelude.Maybe RollbackDetails)
instanceRefresh_rollbackDetails = Lens.lens (\InstanceRefresh' {rollbackDetails} -> rollbackDetails) (\s@InstanceRefresh' {} a -> s {rollbackDetails = a} :: InstanceRefresh)

-- | The date and time at which the instance refresh began.
instanceRefresh_startTime :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.UTCTime)
instanceRefresh_startTime = Lens.lens (\InstanceRefresh' {startTime} -> startTime) (\s@InstanceRefresh' {} a -> s {startTime = a} :: InstanceRefresh) Prelude.. Lens.mapping Data._Time

-- | The current status for the instance refresh operation:
--
-- -   @Pending@ - The request was created, but the instance refresh has
--     not started.
--
-- -   @InProgress@ - An instance refresh is in progress.
--
-- -   @Successful@ - An instance refresh completed successfully.
--
-- -   @Failed@ - An instance refresh failed to complete. You can
--     troubleshoot using the status reason and the scaling activities.
--
-- -   @Cancelling@ - An ongoing instance refresh is being cancelled.
--
-- -   @Cancelled@ - The instance refresh is cancelled.
--
-- -   @RollbackInProgress@ - An instance refresh is being rolled back.
--
-- -   @RollbackFailed@ - The rollback failed to complete. You can
--     troubleshoot using the status reason and the scaling activities.
--
-- -   @RollbackSuccessful@ - The rollback completed successfully.
instanceRefresh_status :: Lens.Lens' InstanceRefresh (Prelude.Maybe InstanceRefreshStatus)
instanceRefresh_status = Lens.lens (\InstanceRefresh' {status} -> status) (\s@InstanceRefresh' {} a -> s {status = a} :: InstanceRefresh)

-- | The explanation for the specific status assigned to this operation.
instanceRefresh_statusReason :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Text)
instanceRefresh_statusReason = Lens.lens (\InstanceRefresh' {statusReason} -> statusReason) (\s@InstanceRefresh' {} a -> s {statusReason = a} :: InstanceRefresh)

instance Data.FromXML InstanceRefresh where
  parseXML x =
    InstanceRefresh'
      Prelude.<$> (x Data..@? "AutoScalingGroupName")
      Prelude.<*> (x Data..@? "DesiredConfiguration")
      Prelude.<*> (x Data..@? "EndTime")
      Prelude.<*> (x Data..@? "InstanceRefreshId")
      Prelude.<*> (x Data..@? "InstancesToUpdate")
      Prelude.<*> (x Data..@? "PercentageComplete")
      Prelude.<*> (x Data..@? "Preferences")
      Prelude.<*> (x Data..@? "ProgressDetails")
      Prelude.<*> (x Data..@? "RollbackDetails")
      Prelude.<*> (x Data..@? "StartTime")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "StatusReason")

instance Prelude.Hashable InstanceRefresh where
  hashWithSalt _salt InstanceRefresh' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` desiredConfiguration
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` instanceRefreshId
      `Prelude.hashWithSalt` instancesToUpdate
      `Prelude.hashWithSalt` percentageComplete
      `Prelude.hashWithSalt` preferences
      `Prelude.hashWithSalt` progressDetails
      `Prelude.hashWithSalt` rollbackDetails
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason

instance Prelude.NFData InstanceRefresh where
  rnf InstanceRefresh' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf desiredConfiguration
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf instanceRefreshId
      `Prelude.seq` Prelude.rnf instancesToUpdate
      `Prelude.seq` Prelude.rnf percentageComplete
      `Prelude.seq` Prelude.rnf preferences
      `Prelude.seq` Prelude.rnf progressDetails
      `Prelude.seq` Prelude.rnf rollbackDetails
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason

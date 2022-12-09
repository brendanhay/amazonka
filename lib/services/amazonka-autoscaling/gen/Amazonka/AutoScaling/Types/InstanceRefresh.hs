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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstanceRefresh where

import Amazonka.AutoScaling.Types.DesiredConfiguration
import Amazonka.AutoScaling.Types.InstanceRefreshProgressDetails
import Amazonka.AutoScaling.Types.InstanceRefreshStatus
import Amazonka.AutoScaling.Types.RefreshPreferences
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
    -- | Describes the specific update you want to deploy.
    desiredConfiguration :: Prelude.Maybe DesiredConfiguration,
    -- | The date and time at which the instance refresh ended.
    endTime :: Prelude.Maybe Data.ISO8601,
    -- | The instance refresh ID.
    instanceRefreshId :: Prelude.Maybe Prelude.Text,
    -- | The number of instances remaining to update before the instance refresh
    -- is complete.
    instancesToUpdate :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of the instance refresh that is complete. For each
    -- instance replacement, Amazon EC2 Auto Scaling tracks the instance\'s
    -- health status and warm-up time. When the instance\'s health status
    -- changes to healthy and the specified warm-up time passes, the instance
    -- is considered updated and is added to the percentage complete.
    percentageComplete :: Prelude.Maybe Prelude.Natural,
    preferences :: Prelude.Maybe RefreshPreferences,
    -- | Additional progress details for an Auto Scaling group that has a warm
    -- pool.
    progressDetails :: Prelude.Maybe InstanceRefreshProgressDetails,
    -- | The date and time at which the instance refresh began.
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The current status for the instance refresh operation:
    --
    -- -   @Pending@ - The request was created, but the operation has not
    --     started.
    --
    -- -   @InProgress@ - The operation is in progress.
    --
    -- -   @Successful@ - The operation completed successfully.
    --
    -- -   @Failed@ - The operation failed to complete. You can troubleshoot
    --     using the status reason and the scaling activities.
    --
    -- -   @Cancelling@ - An ongoing operation is being cancelled. Cancellation
    --     does not roll back any replacements that have already been
    --     completed, but it prevents new replacements from being started.
    --
    -- -   @Cancelled@ - The operation is cancelled.
    status :: Prelude.Maybe InstanceRefreshStatus,
    -- | Provides more details about the current status of the instance refresh.
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
-- 'desiredConfiguration', 'instanceRefresh_desiredConfiguration' - Describes the specific update you want to deploy.
--
-- 'endTime', 'instanceRefresh_endTime' - The date and time at which the instance refresh ended.
--
-- 'instanceRefreshId', 'instanceRefresh_instanceRefreshId' - The instance refresh ID.
--
-- 'instancesToUpdate', 'instanceRefresh_instancesToUpdate' - The number of instances remaining to update before the instance refresh
-- is complete.
--
-- 'percentageComplete', 'instanceRefresh_percentageComplete' - The percentage of the instance refresh that is complete. For each
-- instance replacement, Amazon EC2 Auto Scaling tracks the instance\'s
-- health status and warm-up time. When the instance\'s health status
-- changes to healthy and the specified warm-up time passes, the instance
-- is considered updated and is added to the percentage complete.
--
-- 'preferences', 'instanceRefresh_preferences' - Undocumented member.
--
-- 'progressDetails', 'instanceRefresh_progressDetails' - Additional progress details for an Auto Scaling group that has a warm
-- pool.
--
-- 'startTime', 'instanceRefresh_startTime' - The date and time at which the instance refresh began.
--
-- 'status', 'instanceRefresh_status' - The current status for the instance refresh operation:
--
-- -   @Pending@ - The request was created, but the operation has not
--     started.
--
-- -   @InProgress@ - The operation is in progress.
--
-- -   @Successful@ - The operation completed successfully.
--
-- -   @Failed@ - The operation failed to complete. You can troubleshoot
--     using the status reason and the scaling activities.
--
-- -   @Cancelling@ - An ongoing operation is being cancelled. Cancellation
--     does not roll back any replacements that have already been
--     completed, but it prevents new replacements from being started.
--
-- -   @Cancelled@ - The operation is cancelled.
--
-- 'statusReason', 'instanceRefresh_statusReason' - Provides more details about the current status of the instance refresh.
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
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The name of the Auto Scaling group.
instanceRefresh_autoScalingGroupName :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Text)
instanceRefresh_autoScalingGroupName = Lens.lens (\InstanceRefresh' {autoScalingGroupName} -> autoScalingGroupName) (\s@InstanceRefresh' {} a -> s {autoScalingGroupName = a} :: InstanceRefresh)

-- | Describes the specific update you want to deploy.
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
instanceRefresh_instancesToUpdate :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Natural)
instanceRefresh_instancesToUpdate = Lens.lens (\InstanceRefresh' {instancesToUpdate} -> instancesToUpdate) (\s@InstanceRefresh' {} a -> s {instancesToUpdate = a} :: InstanceRefresh)

-- | The percentage of the instance refresh that is complete. For each
-- instance replacement, Amazon EC2 Auto Scaling tracks the instance\'s
-- health status and warm-up time. When the instance\'s health status
-- changes to healthy and the specified warm-up time passes, the instance
-- is considered updated and is added to the percentage complete.
instanceRefresh_percentageComplete :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Natural)
instanceRefresh_percentageComplete = Lens.lens (\InstanceRefresh' {percentageComplete} -> percentageComplete) (\s@InstanceRefresh' {} a -> s {percentageComplete = a} :: InstanceRefresh)

-- | Undocumented member.
instanceRefresh_preferences :: Lens.Lens' InstanceRefresh (Prelude.Maybe RefreshPreferences)
instanceRefresh_preferences = Lens.lens (\InstanceRefresh' {preferences} -> preferences) (\s@InstanceRefresh' {} a -> s {preferences = a} :: InstanceRefresh)

-- | Additional progress details for an Auto Scaling group that has a warm
-- pool.
instanceRefresh_progressDetails :: Lens.Lens' InstanceRefresh (Prelude.Maybe InstanceRefreshProgressDetails)
instanceRefresh_progressDetails = Lens.lens (\InstanceRefresh' {progressDetails} -> progressDetails) (\s@InstanceRefresh' {} a -> s {progressDetails = a} :: InstanceRefresh)

-- | The date and time at which the instance refresh began.
instanceRefresh_startTime :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.UTCTime)
instanceRefresh_startTime = Lens.lens (\InstanceRefresh' {startTime} -> startTime) (\s@InstanceRefresh' {} a -> s {startTime = a} :: InstanceRefresh) Prelude.. Lens.mapping Data._Time

-- | The current status for the instance refresh operation:
--
-- -   @Pending@ - The request was created, but the operation has not
--     started.
--
-- -   @InProgress@ - The operation is in progress.
--
-- -   @Successful@ - The operation completed successfully.
--
-- -   @Failed@ - The operation failed to complete. You can troubleshoot
--     using the status reason and the scaling activities.
--
-- -   @Cancelling@ - An ongoing operation is being cancelled. Cancellation
--     does not roll back any replacements that have already been
--     completed, but it prevents new replacements from being started.
--
-- -   @Cancelled@ - The operation is cancelled.
instanceRefresh_status :: Lens.Lens' InstanceRefresh (Prelude.Maybe InstanceRefreshStatus)
instanceRefresh_status = Lens.lens (\InstanceRefresh' {status} -> status) (\s@InstanceRefresh' {} a -> s {status = a} :: InstanceRefresh)

-- | Provides more details about the current status of the instance refresh.
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
      Prelude.<*> (x Data..@? "StartTime")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "StatusReason")

instance Prelude.Hashable InstanceRefresh where
  hashWithSalt _salt InstanceRefresh' {..} =
    _salt `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` desiredConfiguration
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` instanceRefreshId
      `Prelude.hashWithSalt` instancesToUpdate
      `Prelude.hashWithSalt` percentageComplete
      `Prelude.hashWithSalt` preferences
      `Prelude.hashWithSalt` progressDetails
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
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason

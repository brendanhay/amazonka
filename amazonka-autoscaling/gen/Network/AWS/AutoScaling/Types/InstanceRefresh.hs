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
-- Module      : Network.AWS.AutoScaling.Types.InstanceRefresh
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceRefresh where

import Network.AWS.AutoScaling.Types.DesiredConfiguration
import Network.AWS.AutoScaling.Types.InstanceRefreshProgressDetails
import Network.AWS.AutoScaling.Types.InstanceRefreshStatus
import Network.AWS.AutoScaling.Types.RefreshPreferences
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an instance refresh for an Auto Scaling group.
--
-- /See:/ 'newInstanceRefresh' smart constructor.
data InstanceRefresh = InstanceRefresh'
  { -- | The current status for the instance refresh operation:
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
    -- | The instance refresh ID.
    instanceRefreshId :: Prelude.Maybe Prelude.Text,
    -- | The percentage of the instance refresh that is complete. For each
    -- instance replacement, Amazon EC2 Auto Scaling tracks the instance\'s
    -- health status and warm-up time. When the instance\'s health status
    -- changes to healthy and the specified warm-up time passes, the instance
    -- is considered updated and is added to the percentage complete.
    percentageComplete :: Prelude.Maybe Prelude.Natural,
    -- | The date and time at which the instance refresh began.
    startTime :: Prelude.Maybe Core.ISO8601,
    -- | The date and time at which the instance refresh ended.
    endTime :: Prelude.Maybe Core.ISO8601,
    -- | Additional progress details for an Auto Scaling group that has a warm
    -- pool.
    progressDetails :: Prelude.Maybe InstanceRefreshProgressDetails,
    -- | Describes the specific update you want to deploy.
    desiredConfiguration :: Prelude.Maybe DesiredConfiguration,
    preferences :: Prelude.Maybe RefreshPreferences,
    -- | Provides more details about the current status of the instance refresh.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The number of instances remaining to update before the instance refresh
    -- is complete.
    instancesToUpdate :: Prelude.Maybe Prelude.Natural,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text
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
-- 'instanceRefreshId', 'instanceRefresh_instanceRefreshId' - The instance refresh ID.
--
-- 'percentageComplete', 'instanceRefresh_percentageComplete' - The percentage of the instance refresh that is complete. For each
-- instance replacement, Amazon EC2 Auto Scaling tracks the instance\'s
-- health status and warm-up time. When the instance\'s health status
-- changes to healthy and the specified warm-up time passes, the instance
-- is considered updated and is added to the percentage complete.
--
-- 'startTime', 'instanceRefresh_startTime' - The date and time at which the instance refresh began.
--
-- 'endTime', 'instanceRefresh_endTime' - The date and time at which the instance refresh ended.
--
-- 'progressDetails', 'instanceRefresh_progressDetails' - Additional progress details for an Auto Scaling group that has a warm
-- pool.
--
-- 'desiredConfiguration', 'instanceRefresh_desiredConfiguration' - Describes the specific update you want to deploy.
--
-- 'preferences', 'instanceRefresh_preferences' - Undocumented member.
--
-- 'statusReason', 'instanceRefresh_statusReason' - Provides more details about the current status of the instance refresh.
--
-- 'instancesToUpdate', 'instanceRefresh_instancesToUpdate' - The number of instances remaining to update before the instance refresh
-- is complete.
--
-- 'autoScalingGroupName', 'instanceRefresh_autoScalingGroupName' - The name of the Auto Scaling group.
newInstanceRefresh ::
  InstanceRefresh
newInstanceRefresh =
  InstanceRefresh'
    { status = Prelude.Nothing,
      instanceRefreshId = Prelude.Nothing,
      percentageComplete = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      progressDetails = Prelude.Nothing,
      desiredConfiguration = Prelude.Nothing,
      preferences = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      instancesToUpdate = Prelude.Nothing,
      autoScalingGroupName = Prelude.Nothing
    }

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

-- | The instance refresh ID.
instanceRefresh_instanceRefreshId :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Text)
instanceRefresh_instanceRefreshId = Lens.lens (\InstanceRefresh' {instanceRefreshId} -> instanceRefreshId) (\s@InstanceRefresh' {} a -> s {instanceRefreshId = a} :: InstanceRefresh)

-- | The percentage of the instance refresh that is complete. For each
-- instance replacement, Amazon EC2 Auto Scaling tracks the instance\'s
-- health status and warm-up time. When the instance\'s health status
-- changes to healthy and the specified warm-up time passes, the instance
-- is considered updated and is added to the percentage complete.
instanceRefresh_percentageComplete :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Natural)
instanceRefresh_percentageComplete = Lens.lens (\InstanceRefresh' {percentageComplete} -> percentageComplete) (\s@InstanceRefresh' {} a -> s {percentageComplete = a} :: InstanceRefresh)

-- | The date and time at which the instance refresh began.
instanceRefresh_startTime :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.UTCTime)
instanceRefresh_startTime = Lens.lens (\InstanceRefresh' {startTime} -> startTime) (\s@InstanceRefresh' {} a -> s {startTime = a} :: InstanceRefresh) Prelude.. Lens.mapping Core._Time

-- | The date and time at which the instance refresh ended.
instanceRefresh_endTime :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.UTCTime)
instanceRefresh_endTime = Lens.lens (\InstanceRefresh' {endTime} -> endTime) (\s@InstanceRefresh' {} a -> s {endTime = a} :: InstanceRefresh) Prelude.. Lens.mapping Core._Time

-- | Additional progress details for an Auto Scaling group that has a warm
-- pool.
instanceRefresh_progressDetails :: Lens.Lens' InstanceRefresh (Prelude.Maybe InstanceRefreshProgressDetails)
instanceRefresh_progressDetails = Lens.lens (\InstanceRefresh' {progressDetails} -> progressDetails) (\s@InstanceRefresh' {} a -> s {progressDetails = a} :: InstanceRefresh)

-- | Describes the specific update you want to deploy.
instanceRefresh_desiredConfiguration :: Lens.Lens' InstanceRefresh (Prelude.Maybe DesiredConfiguration)
instanceRefresh_desiredConfiguration = Lens.lens (\InstanceRefresh' {desiredConfiguration} -> desiredConfiguration) (\s@InstanceRefresh' {} a -> s {desiredConfiguration = a} :: InstanceRefresh)

-- | Undocumented member.
instanceRefresh_preferences :: Lens.Lens' InstanceRefresh (Prelude.Maybe RefreshPreferences)
instanceRefresh_preferences = Lens.lens (\InstanceRefresh' {preferences} -> preferences) (\s@InstanceRefresh' {} a -> s {preferences = a} :: InstanceRefresh)

-- | Provides more details about the current status of the instance refresh.
instanceRefresh_statusReason :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Text)
instanceRefresh_statusReason = Lens.lens (\InstanceRefresh' {statusReason} -> statusReason) (\s@InstanceRefresh' {} a -> s {statusReason = a} :: InstanceRefresh)

-- | The number of instances remaining to update before the instance refresh
-- is complete.
instanceRefresh_instancesToUpdate :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Natural)
instanceRefresh_instancesToUpdate = Lens.lens (\InstanceRefresh' {instancesToUpdate} -> instancesToUpdate) (\s@InstanceRefresh' {} a -> s {instancesToUpdate = a} :: InstanceRefresh)

-- | The name of the Auto Scaling group.
instanceRefresh_autoScalingGroupName :: Lens.Lens' InstanceRefresh (Prelude.Maybe Prelude.Text)
instanceRefresh_autoScalingGroupName = Lens.lens (\InstanceRefresh' {autoScalingGroupName} -> autoScalingGroupName) (\s@InstanceRefresh' {} a -> s {autoScalingGroupName = a} :: InstanceRefresh)

instance Core.FromXML InstanceRefresh where
  parseXML x =
    InstanceRefresh'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "InstanceRefreshId")
      Prelude.<*> (x Core..@? "PercentageComplete")
      Prelude.<*> (x Core..@? "StartTime")
      Prelude.<*> (x Core..@? "EndTime")
      Prelude.<*> (x Core..@? "ProgressDetails")
      Prelude.<*> (x Core..@? "DesiredConfiguration")
      Prelude.<*> (x Core..@? "Preferences")
      Prelude.<*> (x Core..@? "StatusReason")
      Prelude.<*> (x Core..@? "InstancesToUpdate")
      Prelude.<*> (x Core..@? "AutoScalingGroupName")

instance Prelude.Hashable InstanceRefresh

instance Prelude.NFData InstanceRefresh

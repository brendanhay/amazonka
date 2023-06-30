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
-- Module      : Amazonka.Evidently.Types.Launch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.Launch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.LaunchExecution
import Amazonka.Evidently.Types.LaunchGroup
import Amazonka.Evidently.Types.LaunchStatus
import Amazonka.Evidently.Types.LaunchType
import Amazonka.Evidently.Types.MetricMonitor
import Amazonka.Evidently.Types.ScheduledSplitsLaunchDefinition
import qualified Amazonka.Prelude as Prelude

-- | This structure contains the configuration details of one Evidently
-- launch.
--
-- /See:/ 'newLaunch' smart constructor.
data Launch = Launch'
  { -- | The description of the launch.
    description :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains information about the start and end times of
    -- the launch.
    execution :: Prelude.Maybe LaunchExecution,
    -- | An array of structures that define the feature variations that are being
    -- used in the launch.
    groups :: Prelude.Maybe [LaunchGroup],
    -- | An array of structures that define the metrics that are being used to
    -- monitor the launch performance.
    metricMonitors :: Prelude.Maybe [MetricMonitor],
    -- | The name or ARN of the project that contains the launch.
    project :: Prelude.Maybe Prelude.Text,
    -- | This value is used when Evidently assigns a particular user session to
    -- the launch, to help create a randomization ID to determine which
    -- variation the user session is served. This randomization ID is a
    -- combination of the entity ID and @randomizationSalt@.
    randomizationSalt :: Prelude.Maybe Prelude.Text,
    -- | An array of structures that define the traffic allocation percentages
    -- among the feature variations during each step of the launch.
    scheduledSplitsDefinition :: Prelude.Maybe ScheduledSplitsLaunchDefinition,
    -- | If the launch was stopped, this is the string that was entered by the
    -- person who stopped the launch, to explain why it was stopped.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The list of tag keys and values associated with this launch.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the launch.
    arn :: Prelude.Text,
    -- | The date and time that the launch is created.
    createdTime :: Data.POSIX,
    -- | The date and time that the launch was most recently updated.
    lastUpdatedTime :: Data.POSIX,
    -- | The name of the launch.
    name :: Prelude.Text,
    -- | The current state of the launch.
    status :: LaunchStatus,
    -- | The type of launch.
    type' :: LaunchType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Launch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'launch_description' - The description of the launch.
--
-- 'execution', 'launch_execution' - A structure that contains information about the start and end times of
-- the launch.
--
-- 'groups', 'launch_groups' - An array of structures that define the feature variations that are being
-- used in the launch.
--
-- 'metricMonitors', 'launch_metricMonitors' - An array of structures that define the metrics that are being used to
-- monitor the launch performance.
--
-- 'project', 'launch_project' - The name or ARN of the project that contains the launch.
--
-- 'randomizationSalt', 'launch_randomizationSalt' - This value is used when Evidently assigns a particular user session to
-- the launch, to help create a randomization ID to determine which
-- variation the user session is served. This randomization ID is a
-- combination of the entity ID and @randomizationSalt@.
--
-- 'scheduledSplitsDefinition', 'launch_scheduledSplitsDefinition' - An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of the launch.
--
-- 'statusReason', 'launch_statusReason' - If the launch was stopped, this is the string that was entered by the
-- person who stopped the launch, to explain why it was stopped.
--
-- 'tags', 'launch_tags' - The list of tag keys and values associated with this launch.
--
-- 'arn', 'launch_arn' - The ARN of the launch.
--
-- 'createdTime', 'launch_createdTime' - The date and time that the launch is created.
--
-- 'lastUpdatedTime', 'launch_lastUpdatedTime' - The date and time that the launch was most recently updated.
--
-- 'name', 'launch_name' - The name of the launch.
--
-- 'status', 'launch_status' - The current state of the launch.
--
-- 'type'', 'launch_type' - The type of launch.
newLaunch ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'lastUpdatedTime'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  LaunchStatus ->
  -- | 'type''
  LaunchType ->
  Launch
newLaunch
  pArn_
  pCreatedTime_
  pLastUpdatedTime_
  pName_
  pStatus_
  pType_ =
    Launch'
      { description = Prelude.Nothing,
        execution = Prelude.Nothing,
        groups = Prelude.Nothing,
        metricMonitors = Prelude.Nothing,
        project = Prelude.Nothing,
        randomizationSalt = Prelude.Nothing,
        scheduledSplitsDefinition = Prelude.Nothing,
        statusReason = Prelude.Nothing,
        tags = Prelude.Nothing,
        arn = pArn_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        lastUpdatedTime =
          Data._Time Lens.# pLastUpdatedTime_,
        name = pName_,
        status = pStatus_,
        type' = pType_
      }

-- | The description of the launch.
launch_description :: Lens.Lens' Launch (Prelude.Maybe Prelude.Text)
launch_description = Lens.lens (\Launch' {description} -> description) (\s@Launch' {} a -> s {description = a} :: Launch)

-- | A structure that contains information about the start and end times of
-- the launch.
launch_execution :: Lens.Lens' Launch (Prelude.Maybe LaunchExecution)
launch_execution = Lens.lens (\Launch' {execution} -> execution) (\s@Launch' {} a -> s {execution = a} :: Launch)

-- | An array of structures that define the feature variations that are being
-- used in the launch.
launch_groups :: Lens.Lens' Launch (Prelude.Maybe [LaunchGroup])
launch_groups = Lens.lens (\Launch' {groups} -> groups) (\s@Launch' {} a -> s {groups = a} :: Launch) Prelude.. Lens.mapping Lens.coerced

-- | An array of structures that define the metrics that are being used to
-- monitor the launch performance.
launch_metricMonitors :: Lens.Lens' Launch (Prelude.Maybe [MetricMonitor])
launch_metricMonitors = Lens.lens (\Launch' {metricMonitors} -> metricMonitors) (\s@Launch' {} a -> s {metricMonitors = a} :: Launch) Prelude.. Lens.mapping Lens.coerced

-- | The name or ARN of the project that contains the launch.
launch_project :: Lens.Lens' Launch (Prelude.Maybe Prelude.Text)
launch_project = Lens.lens (\Launch' {project} -> project) (\s@Launch' {} a -> s {project = a} :: Launch)

-- | This value is used when Evidently assigns a particular user session to
-- the launch, to help create a randomization ID to determine which
-- variation the user session is served. This randomization ID is a
-- combination of the entity ID and @randomizationSalt@.
launch_randomizationSalt :: Lens.Lens' Launch (Prelude.Maybe Prelude.Text)
launch_randomizationSalt = Lens.lens (\Launch' {randomizationSalt} -> randomizationSalt) (\s@Launch' {} a -> s {randomizationSalt = a} :: Launch)

-- | An array of structures that define the traffic allocation percentages
-- among the feature variations during each step of the launch.
launch_scheduledSplitsDefinition :: Lens.Lens' Launch (Prelude.Maybe ScheduledSplitsLaunchDefinition)
launch_scheduledSplitsDefinition = Lens.lens (\Launch' {scheduledSplitsDefinition} -> scheduledSplitsDefinition) (\s@Launch' {} a -> s {scheduledSplitsDefinition = a} :: Launch)

-- | If the launch was stopped, this is the string that was entered by the
-- person who stopped the launch, to explain why it was stopped.
launch_statusReason :: Lens.Lens' Launch (Prelude.Maybe Prelude.Text)
launch_statusReason = Lens.lens (\Launch' {statusReason} -> statusReason) (\s@Launch' {} a -> s {statusReason = a} :: Launch)

-- | The list of tag keys and values associated with this launch.
launch_tags :: Lens.Lens' Launch (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
launch_tags = Lens.lens (\Launch' {tags} -> tags) (\s@Launch' {} a -> s {tags = a} :: Launch) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the launch.
launch_arn :: Lens.Lens' Launch Prelude.Text
launch_arn = Lens.lens (\Launch' {arn} -> arn) (\s@Launch' {} a -> s {arn = a} :: Launch)

-- | The date and time that the launch is created.
launch_createdTime :: Lens.Lens' Launch Prelude.UTCTime
launch_createdTime = Lens.lens (\Launch' {createdTime} -> createdTime) (\s@Launch' {} a -> s {createdTime = a} :: Launch) Prelude.. Data._Time

-- | The date and time that the launch was most recently updated.
launch_lastUpdatedTime :: Lens.Lens' Launch Prelude.UTCTime
launch_lastUpdatedTime = Lens.lens (\Launch' {lastUpdatedTime} -> lastUpdatedTime) (\s@Launch' {} a -> s {lastUpdatedTime = a} :: Launch) Prelude.. Data._Time

-- | The name of the launch.
launch_name :: Lens.Lens' Launch Prelude.Text
launch_name = Lens.lens (\Launch' {name} -> name) (\s@Launch' {} a -> s {name = a} :: Launch)

-- | The current state of the launch.
launch_status :: Lens.Lens' Launch LaunchStatus
launch_status = Lens.lens (\Launch' {status} -> status) (\s@Launch' {} a -> s {status = a} :: Launch)

-- | The type of launch.
launch_type :: Lens.Lens' Launch LaunchType
launch_type = Lens.lens (\Launch' {type'} -> type') (\s@Launch' {} a -> s {type' = a} :: Launch)

instance Data.FromJSON Launch where
  parseJSON =
    Data.withObject
      "Launch"
      ( \x ->
          Launch'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "execution")
            Prelude.<*> (x Data..:? "groups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "metricMonitors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "project")
            Prelude.<*> (x Data..:? "randomizationSalt")
            Prelude.<*> (x Data..:? "scheduledSplitsDefinition")
            Prelude.<*> (x Data..:? "statusReason")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdTime")
            Prelude.<*> (x Data..: "lastUpdatedTime")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable Launch where
  hashWithSalt _salt Launch' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` execution
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` metricMonitors
      `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` randomizationSalt
      `Prelude.hashWithSalt` scheduledSplitsDefinition
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Launch where
  rnf Launch' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf execution
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf metricMonitors
      `Prelude.seq` Prelude.rnf project
      `Prelude.seq` Prelude.rnf randomizationSalt
      `Prelude.seq` Prelude.rnf scheduledSplitsDefinition
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
